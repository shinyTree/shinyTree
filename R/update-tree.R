#' Update the tree with new data
#' 
#' Extract the nodes from the tree that are selected in a more
#' convenient format. You can choose which format you prefer.
#' 
#' @param session The current session variable.
#' @param treeId The identifier for the shinyTree object
#' @param data JSON data or nested list representing the new tree structure.
#' @export
updateTree <- function(session, treeId, data=NULL) {
  if(is.list(data)){
    data<-Rlist2json(data)
  }
  message <- list(type="updateTree",data=data)
  if(!is.null(message)) {
    session$sendInputMessage(treeId, message)
  }
}


#' Expand or collapse every node of a tree
#'
#' Opens or closes every node of a \code{\link{shinyTree}} in the browser,
#' for example from "Expand all" and "Collapse all" buttons. The tree's
#' contents are not resent, so selection, checkboxes and search are kept, and
#' \code{input$tree} reports each node's new open state as it would after a
#' user click.
#'
#' To control which nodes start open instead, set \code{stopened = TRUE} on
#' them when rendering the tree.
#'
#' @param session The current session variable.
#' @param treeId The identifier for the shinyTree object.
#' @examples
#' \dontrun{
#' ui <- fluidPage(
#'   actionButton("expand", "Expand all"),
#'   actionButton("collapse", "Collapse all"),
#'   shinyTree("tree")
#' )
#' server <- function(input, output, session) {
#'   output$tree <- renderTree(list(a = list(b = list(c = ""))))
#'   observeEvent(input$expand, expandTree(session, "tree"))
#'   observeEvent(input$collapse, collapseTree(session, "tree"))
#' }
#' }
#' @seealso \code{\link{updateTree}}
#' @export
expandTree <- function(session, treeId) {
  session$sendInputMessage(treeId, list(type = "openAll"))
}

#' @rdname expandTree
#' @export
collapseTree <- function(session, treeId) {
  session$sendInputMessage(treeId, list(type = "closeAll"))
}

#' @importFrom jsonlite toJSON
Rlist2json <- function(nestedList) {
  flatList <- get_flatList(nestedList)
  as.character(toJSON(flatListToFrame(flatList) %||% flatList, auto_unbox = T))
}

# jsonlite serialises a list of nodes one element at a time, which dominated
# rendering for large trees (#83). A data frame with one row per node serialises
# column-wise and produces the same JSON: a row omits a field whose value is NA,
# just as a node omits an icon or type it does not have. Returns NULL when that
# equivalence does not hold, so the caller falls back to the list.
`%||%` <- function(x, y) if (is.null(x)) y else x

flatListToFrame <- function(flatList) {
  if (!length(flatList)) {
    return(NULL)
  }
  text <- lapply(flatList, `[[`, "text")
  # A missing or NA name is serialised differently by the two forms.
  if (!all(vapply(text, function(x) is.character(x) && length(x) == 1 && !is.na(x), logical(1)))) {
    return(NULL)
  }
  field <- function(name) {
    vapply(flatList, function(x) x[[name]] %||% NA_character_, character(1))
  }
  state <- function(name) {
    vapply(flatList, function(x) x$state[[name]], logical(1))
  }
  frame <- data.frame(id = field("id"), text = unlist(text), parent = field("parent"),
                      stringsAsFactors = FALSE)
  frame$state <- data.frame(opened = state("opened"), selected = state("selected"),
                            disabled = state("disabled"), checked = state("checked"))
  frame$data <- lapply(flatList, `[[`, "data")
  frame$icon <- field("icon")
  frame$type <- field("type")
  frame
}

#' @importFrom stringr str_match
# fix icon retains backward compatibility for icon entries that are not fully specified
fixIconName <- function(icon){
  ## - 'yes' branch of 'if' covers everything which should not be changed
  ##   e.g. "/images/ball.jpg" or "fa fa-file
  ## - 'no' branch of 'if' covers all cases which need to be changed:
  ##   use regex (str_match) to capture groups: 
  ##     * group 1 is either 'glyphicon', 'fa' or 'NA' (if not present)
  ##     * group 2 is the rest wihtout a potential dash '-'
  ##     * if group 1 is empty set it to 'fa'
  ##     * paste the pieces together
  res <- ifelse(grepl("[/\\]|(glyphicon|fa) \\1-", icon), 
                icon, 
                {
                  parts <- str_match(icon, "(glyphicon|fa)*-*(\\S+)")
                  parts[, 2] <- ifelse(is.na(parts[, 2]), "fa", parts[, 2])
                  paste(parts[, 2], paste(parts[, 2], parts[, 3], sep = "-")) 
                })
  ## if NULL was given as parameter res will be length zero
  if (!length(res)) {
    NULL
  } else {
    res
  }
}

get_flatList <- function(nestedList, flatList = NULL, parent = "#") {
  # Nodes are written into a preallocated list in depth-first order, so a
  # node's position is also its id. Growing the result with c() copied the whole
  # list once per node, which made rendering quadratic in the size of the tree
  # (#83).
  out <- vector("list", countNodes(nestedList))
  n <- 0L

  walk <- function(nestedList, parent) {
    # Walk by position rather than by name: `nestedList[[name]]` always resolves
    # to the first match, which smeared that node's attributes onto every later
    # sibling sharing its name (#104).
    nodeNames <- names(nestedList)
    for (i in seq_along(nestedList)) {
      node <- nestedList[[i]]
      n <<- n + 1L
      id <- as.character(n)
      out[[n]] <<- flatNode(node, nodeNames[i], id, parent)
      if (is.list(node)) {
        walk(node, id)
      }
    }
  }
  walk(nestedList, parent)

  c(flatList, out)
}

countNodes <- function(nestedList) {
  total <- length(nestedList)
  for (node in nestedList) {
    if (is.list(node)) {
      total <- total + countNodes(node)
    }
  }
  total
}

# Attributes that shinyTree interprets itself rather than passing through as
# node data.
reservedAttributes <- c("icon", "type", "names", "stopened", "stselected",
                        "sttype", "stdisabled", "stchecked")

flatNode <- function(node, name, id, parent) {
  attrs <- attributes(node)

  # A node with no attributes at all yields an unnamed list(), serialised as
  # [], while one whose attributes are all reserved yields an empty named list,
  # serialised as {}. Both are kept as they were.
  data <- list()
  if (length(attrs) > 0) {
    data <- attrs[!names(attrs) %in% reservedAttributes]
    data <- data[!vapply(data, is.null, logical(1))]
  }

  nodeData <- list(
    id = id,
    text = name,
    parent = parent,
    state = list(
      opened   = isTRUE(attrs$stopened),
      selected = isTRUE(attrs$stselected),
      disabled = isTRUE(attrs$stdisabled),
      checked  = isTRUE(attrs$stchecked)
    ),
    data = data
  )
  if (!is.null(attrs$sticon)) {
    nodeData$icon <- fixIconName(attrs$sticon)
  }
  if (!is.null(attrs$sttype)) {
    nodeData$type <- attrs$sttype
  }
  nodeData
}
