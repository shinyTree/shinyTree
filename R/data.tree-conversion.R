#' Converts a data.tree to a JSON format
#' 
#' Walk through a \code{\link[data.tree]{data.tree}} and constructs a JSON string,
#' which can be rendered by shinyTree. 
#' 
#' The JSON string generated follows the 
#' \href{https://www.jstree.com/docs/json}{jsTree specifications}. In particular 
#' it encodes children nodes via the \sQuote{children} slot. 
#' 
#' All atomic or list slots of a node in the tree are stored in a data slot in 
#' the resulting JSON.
#' 
#' If the user wants to store some slots not in the data slot but on the top 
#' level of the node, parameter \code{topLevelSlots} can be used. This is useful
#' for additional parameters such as \sQuote{icon}, \sQuote{li_attr} or 
#' \sQuote{a_attr}, which jsTree expect to be on the top level of the node.
#' 
#' An example of how to make use of this functionality can be found in the
#' example folder of this library.
#'
#' @param tree the data.tree which should be parses
#' @param keepRoot logical. If \code{FALSE} (default) the root node from the tree
#'        is pruned
#' @param topLevelSlots determines which slots should be moved to the top level of the 
#'        node. If \code{default} or \code{NULL} slots 
#'        \href{https://www.jstree.com/docs/json}{used in the jsTree JSON} 
#'        are kept on the top level, while any other atomic / list slots from the tree
#'        are stored in an own slot called \sQuote{data}. If \code{all} *all* nodes are
#'        stored on the top level. Alternatively, it can be an explicit vector of
#'        slot names which should be kept. In the latter case it is the user's
#'        responsibility to ensure that jsTree slots stay on the top level.
#' @param createNewId logical. If \code{TRUE} a new id will be generated. Any old \sQuote{id} 
#'        will be stored in \sQuote{id.orig} and a warning will be issued, If \code{FALSE},
#'        any existing id will be re-used.
#' @param pretty logical. If \code{TRUE} the resulting JSON is prettified
#' @return a JSON string representing the data.tree
#' @section Note:
#' \code{\link{updateTree}} and \code{\link{renderTree}} need an unevaluated JSON 
#' string. Hence, this function returns a string rather than the JSON object itself.
#' @author Thorn Thaler, \email{thorn.thaler@@thothal.at} 
#' @export
treeToJSON <- function(tree, 
                       keepRoot = FALSE,
                       topLevelSlots = c("default", "all"),
                       createNewId = TRUE,
                       pretty = FALSE) {
  ## match against "default"/"all", if this returns an error we take topLevelSlots as is
  ## i.e. a vector of names to keep
  if (!requireNamespace("data.tree", quietly = TRUE)) {
    msg <- paste("library", sQuote("data.tree"), "cannot be loaded. Try to run",
                 sQuote("install.packages(\"data.tree\")"))
    stop(msg, domain = NA)
  }
  nodesToKeep <- list(default = c("id", "text", "icon", "state",
                                  "li_attr", "a_attr", "type"),
                      all     = NULL)
  topLevelSlots <- tryCatch(nodesToKeep[[match.arg(topLevelSlots)]],
                            error = function(e) topLevelSlots)
  node_to_list <- function(node, 
                           node_name = NULL) {
    fields <- mget(node$fields, node)
    NOK <- sapply(fields, function(slot) !is.atomic(slot) && !is.list(slot))
    if (any(NOK)) {
      msg <- sprintf(ngettext(length(which(NOK)),
                              "unsupported slot of type %s at position %s",
                              "unsupported slots of types %s at positions %s"),
                     paste0(dQuote(sapply(fields[NOK], typeof)),
                            collapse = ", "),
                     paste0(sQuote(names(fields)[NOK]),
                            collapse = ", "))
      warning(msg,
              domain = NA)
      fields[NOK] <- NULL
    }
    if (is.null(fields$text)) {
      fields$text <- if(!is.null(fields$name)) fields$name else node_name
    }
    fields$icon <- fixIconName(fields$icon)
    if (!is.null(fields$state)) {
      valid_states <- c("opened", "disabled", "selected", "loaded")
      states_template <- stats::setNames(rep(list(FALSE), length(valid_states)),
                                         valid_states)
      NOK <- !names(fields$state) %in% valid_states
      if (any(NOK)) {
        msg <- sprintf(ngettext(length(which(NOK)),
                                "invalid state %s",
                                "invalid states %s"),
                       paste0(dQuote(names(fields$state)[NOK]),
                              collapse = ", "))
        warning(msg,
                domain = NA)
      }
      states_template[names(fields$state[!NOK])] <- fields$state[!NOK]
      fields$state <- states_template
    }
    if (is.null(topLevelSlots)) {
      slots_to_move <- character(0)
    } else {
      slots_to_move <- names(fields)[!names(fields) %in% topLevelSlots]
    }
    data_slot <- fields[slots_to_move]
    if (length(data_slot)) {
      fields$data <- data_slot
      fields[slots_to_move] <- NULL
    }
    if (!is.null(node$children)) {
      ## purrr::imap would make code cleaner but did not want to add another dependency
      ## unname needed to create an JSON array as opposed to an JSON object
      fields$children <- unname(lapply(names(node$children), 
                                       function(i) node_to_list(node$children[[i]],
                                                                i)))
    }
    fields
  }
  ## clone tree as we do not want to alter the original tree
  tree <- data.tree::Clone(tree)
  nodes <- data.tree::Traverse(tree, filterFun = data.tree::isNotRoot)
  old_ids <- data.tree::Get(nodes, "id")
  if (createNewId) {
    if (any(!is.na(old_ids))) {
      warning(paste("slot",
                    dQuote("id"), 
                    "will be stored in",
                    dQuote("id.orig")),
              domain = NA)
      data.tree::Set(nodes, id.orig = old_ids)
    }
    new_ids <- seq_along(nodes)
  } else {
    if (any(is.na(old_ids)) ||
        any(duplicated(old_ids))) {
      warning(paste("old ids are invalid (duplicated values or NA),",
                    "creating new ids"),
              domain = NA)
      new_ids <- seq_along(nodes)
    } else {
      new_ids <- old_ids
    }
  }

  data.tree::Set(nodes, id = new_ids)
  treeList <- node_to_list(tree)
  if (!keepRoot) {
    ## to prune off the root node return the first children list
    treeList <- treeList$children
  }
  ## use as.character b/c updateTree needs an unparsed JSON string, as 
  ## the parsing is done in shinyTree.js
  as.character(jsonlite::toJSON(treeList, 
                                auto_unbox = TRUE, 
                                pretty = pretty))
}


#' Recursively apply function to all data.frames in a nested list
#' 
#' @param list (nested) list containing data.frames 
#' @param f function to apply to each data.frame
#' @param ... extra arguments to f
#' @return list
#' 
#' @author Jasper Schelfhout \email{jasper.schelfhout@@openanalytics.eu}
dfrapply <- function(list, f, ...) {
  if (inherits(list, "data.frame")) {
    return(f(list, ...))
  }
  if (inherits(list, "list")) {
    return(lapply(list, function(x) dfrapply(x, f, ...)))
  }
  stop("List element must be either a data frame or another list")
}


#' Converts a data.frame  to a data.tree format
#' 
#' @param df data.frame
#' @param hierarchy ordered character vector of column names defining the hierarchy
#' @examples
#'\dontrun{
#' df <- data.frame(Titanic)
#' tree <- dfToTree(df, c("Sex", "Class", "Survived"))
#'}
#' @author Jasper Schelfhout \email{jasper.schelfhout@@openanalytics.eu}
#' @return nested list
#' @export
dfToTree <- function(
    df,
    hierarchy = colnames(df)){ 
  l <- df
  for(c in hierarchy){
    l <- dfrapply(
        list = l,
        f = function(x){
          split(x, x[[c]], drop = TRUE)
        }
    )
  }
  dfrapply(l, function(x){""})
}

#' Convert tree into data.frame
#' 
#' @param tree named nested list
#' @param hierarchy sorted character vector with name for each level of the list
#' @examples
#'\dontrun{
#' df <- data.frame(Titanic)
#' tree <- dfToTree(df, c("Sex", "Class", "Survived"))
#' newDf <- treeToDf(tree, c("Sex", "Class", "Survived"))
#'}
#' @return data.frame
#' 
#' @author Jasper Schelfhout
#' @export
treeToDf <- function(tree, hierarchy){
  depth <- depth(tree) 
  
  if(depth > length(hierarchy)){
    stop("Not enough names specified in hierarchy.")
  }
  
  if(depth < length(hierarchy)){
    hierarchy <- tail(hierarchy, depth)    
    warning(sprintf("To many levels specified in hierarchy. Only using last %s: %s",
            depth,
            paste(hierarchy, collapse = ", ")
        )
    )
  }
  
  for(i in seq_len(length(hierarchy))){
    tree <- nodesToDf(tree, hierarchy)
    hierarchy <- hierarchy[-length(hierarchy)]
  }
  df <- tree
}

#' Convert the nodes of a tree into a data.frame
#' 
#' @param tree named nested list
#' @param hierarchy sorted character vector with name for each level of the list
#' @return nested list with one level less that is stacked to a data.frame
#' 
#' @author Jasper Schelfhout \email{jasper.schelfhout@@openanalytics.eu}
nodesToDf <- function(
    tree,
    hierarchy){
  depth <- depth(tree) 
  if(depth == 0 || depth < length(hierarchy)){
    return(tree)
  }
  if(depth == 1){
    df <- stackList(tree, hierarchy)
  } else if(depth > 1){
    df <- lapply(
        tree,
        treeToDf, 
        hierarchy = tail(hierarchy, depth-1))
  }
  df
}

#' Combine named list into single data.frame with extra column
#' 
#' @param list named list
#' @param name character
#' @return data.frame
#' 
#' @author Jasper Schelfhout \email{jasper.schelfhout@@openanalytics.eu}
stackList <- function(list, name){
  outputList <- lapply(names(list),
      function(x, name){
        if(inherits(list[[x]], "data.frame")){
          df <- list[[x]]
          df[,name] <- x
        } else {
          df <- data.frame(x, stringsAsFactors = FALSE)
          names(df) = name   
        }
        df
      },
      name = name)
  Reduce(function(x,y){merge(x,y,all=TRUE)}, outputList)
}

#' Check depth of a list
#' 
#' @param x list
#' @return integer
#' 
#' @author Jasper Schelfhout \email{jasper.schelfhout@@openanalytics.eu}
depth <- function(x){
  ifelse(is.list(x) && !is.data.frame(x), 1L + max(sapply(x, depth)), 0L)
}
