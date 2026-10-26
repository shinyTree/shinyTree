#' Update the tree with new data
#' 
#' Replaces the contents of a \code{\link{shinyTree}} with a new tree, for
#' example from an observer. Like the tree \code{\link{renderTree}} sends, the
#' new one is reported in \code{input$tree} once it has loaded -- unless it is
#' the tree \code{input$tree} already holds, so an observer of
#' \code{input$tree} that sends the tree back does not loop.
#'
#' A tree rendered with \code{renderTree(lazy = TRUE)} cannot be updated this
#' way, and \code{updateTree} stops with an error: change what its render
#' expression returns instead. If the tree has not been shown yet -- it is in
#' a tab that has not been opened -- the update is applied when it is.
#' 
#' @param session The current session variable.
#' @param treeId The identifier for the shinyTree object
#' @param data The new tree: a named nested list, as for
#'   \code{\link{renderTree}}, or JSON in jsTree's format, as from
#'   \code{\link{treeToJSON}}. \code{NULL} leaves the tree as it is.
#' @return Called for its side effect; returns \code{NULL} invisibly.
#' @examples
#' \dontrun{
#' observeEvent(input$reset, {
#'   updateTree(session, "tree", list(root = list(leaf = "")))
#' })
#' }
#' @seealso \code{\link{renderTree}}, \code{\link{expandTree}}
#' @export
updateTree <- function(session, treeId, data=NULL) {
  stopIfLazy(session, treeId, "updateTree",
             "Change what its render expression returns to rebuild it instead.")
  session$sendInputMessage(treeId, list(type = "updateTree", data = treeJson(data)))
  invisible()
}

# The JSON string the browser parses for a tree: a nested list is converted,
# and JSON -- a string, or a "json" object from jsonlite::toJSON() -- is sent
# as a string. A "json" object would otherwise reach the browser already
# parsed, as Shiny embeds it verbatim, and fail to parse again there. NULL
# stays NULL, which leaves the tree as it is.
treeJson <- function(data) {
  if (is.list(data)) {
    Rlist2json(data)
  } else if (is.null(data)) {
    NULL
  } else {
    as.character(data)
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
#' \code{expandTree} cannot be used on a tree rendered with
#' \code{renderTree(lazy = TRUE)}, since it would have to fetch the whole
#' tree, and stops with an error. \code{collapseTree} works on any tree.
#'
#' @param session The current session variable.
#' @param treeId The identifier for the shinyTree object.
#' @return Called for its side effect; returns \code{NULL} invisibly.
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
  stopIfLazy(session, treeId, "expandTree",
             "It would have to fetch the whole tree, which may be endless.")
  session$sendInputMessage(treeId, list(type = "openAll"))
  invisible()
}

#' @rdname expandTree
#' @export
collapseTree <- function(session, treeId) {
  session$sendInputMessage(treeId, list(type = "closeAll"))
  invisible()
}

#' @importFrom jsonlite toJSON
Rlist2json <- function(nestedList) {
  flatList <- get_flatList(nestedList)
  # digits = NA: node data is sent at full precision, not rounded to four
  # digits, so a numeric id or value comes back in input$tree as it was.
  as.character(toJSON(flatListToFrame(flatList) %||% flatList, auto_unbox = TRUE,
                      digits = NA))
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
  # A field that is not a single string for every node -- a numeric sttype,
  # say -- makes the frame unusable, and the list is serialised instead.
  isString <- function(x) is.null(x) || (is.character(x) && length(x) == 1)
  fieldOk <- function(get) all(vapply(flatList, function(x) isString(get(x)), logical(1)))
  if (!fieldOk(function(x) x$icon) || !fieldOk(function(x) x$type) ||
      !fieldOk(function(x) x$a_attr$title)) {
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
  # Only added when some node has a tooltip, so trees without one serialise
  # exactly as before. A node without one gets an empty a_attr, which jsTree
  # ignores.
  title <- vapply(flatList, function(x) x$a_attr$title %||% NA_character_, character(1))
  if (any(!is.na(title))) {
    frame$a_attr <- data.frame(title = title, stringsAsFactors = FALSE)
  }
  cssClass <- vapply(flatList, function(x) x$li_attr$class %||% NA_character_, character(1))
  if (any(!is.na(cssClass))) {
    frame$li_attr <- data.frame(class = cssClass, stringsAsFactors = FALSE)
  }
  frame
}

# Expand an icon given by its name alone to the classes that show it, for
# backward compatibility: "file" and "fa-file" become "fa fa-file", and
# "glyphicon-file" becomes "glyphicon glyphicon-file". Anything else is left as
# it is -- several classes ("fa-solid fa-folder", "fas fa-file", "fa-file
# fa-lg", "bi bi-folder") or an image path or URL ("ball.png",
# "/images/ball.jpg") -- since it already says exactly what to show.
fixIconName <- function(icon){
  if (is.null(icon)) {
    return(NULL)
  }
  icon <- as.character(icon)
  bare <- !is.na(icon) & grepl("^[A-Za-z0-9_-]+$", icon)
  glyph <- bare & startsWith(icon, "glyphicon-")
  fa <- bare & !glyph
  icon[glyph] <- paste("glyphicon", icon[glyph])
  name <- sub("^fa-", "", icon[fa])
  icon[fa] <- paste0("fa fa-", name)
  if (!length(icon)) NULL else icon
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

# Attributes that shinyTree interprets itself, or that R gives a node of its
# own -- a list's names, a data frame's class and row names -- rather than
# passing through as node data.
reservedAttributes <- c("icon", "type", "names", "stopened", "stselected",
                        "sttype", "stdisabled", "stchecked", "sttooltip",
                        "class", "row.names", "dim", "dimnames", "levels", "tsp")

flatNode <- function(node, name, id, parent) {
  if (is.null(name) || is.na(name) || !nzchar(name)) {
    stop("Every node of a tree needs a name: the list passed to renderTree() or ",
         "updateTree() has an element without one.", call. = FALSE)
  }
  attrs <- attributes(node)
  # [[ rather than $, which would take any attribute whose name merely starts
  # with the one asked for: sttype_label for sttype, say.
  attr1 <- function(name) attrs[[name, exact = TRUE]]

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
      opened   = isTRUE(attr1("stopened")),
      selected = isTRUE(attr1("stselected")),
      disabled = isTRUE(attr1("stdisabled")),
      checked  = isTRUE(attr1("stchecked"))
    ),
    data = data
  )
  icon <- fixIconName(oneString(attr1("sticon")))
  if (!is.null(icon)) {
    nodeData$icon <- icon
  }
  type <- oneString(attr1("sttype"))
  if (!is.null(type)) {
    nodeData$type <- type
  }
  # Shown by the browser when the pointer rests on the node's label (#66).
  tooltip <- attr1("sttooltip")
  if (!is.null(tooltip)) {
    tooltip <- as.character(tooltip)
    nodeData$a_attr <- list(title = paste(tooltip[!is.na(tooltip)], collapse = "\n"))
  }
  # CSS classes on the node's <li>, so a stylesheet can style it (#58). Any
  # number may be given, as a vector or a space-separated string. stclass is
  # still passed through as data too, so get_selected(format = "classid")
  # keeps returning it.
  cssClass <- nodeCssClass(attr1("stcssclass"))
  if (!is.null(cssClass)) {
    nodeData$li_attr <- list(class = cssClass)
  }
  nodeData
}

# The space-separated class string for stcssclass, or NULL if there is none.
nodeCssClass <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }
  classes <- unlist(strsplit(as.character(unlist(x)), "[[:space:]]+"))
  classes <- unique(classes[!is.na(classes) & nzchar(classes)])
  if (!length(classes)) {
    return(NULL)
  }
  paste(classes, collapse = " ")
}

# A single string from an attribute value, or NULL: a factor becomes its label
# and a number its digits, the first element is used, and NA counts as unset.
oneString <- function(x) {
  if (is.null(x) || !length(x)) {
    return(NULL)
  }
  x <- as.character(unlist(x))[1]
  if (is.na(x) || !nzchar(x)) NULL else x
}
