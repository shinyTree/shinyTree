#' Get the selected nodes from a tree
#' 
#' Extract the nodes from the tree that are selected in a more
#' convenient format. You can choose which format you prefer.
#' 
#' @param tree The \code{input$tree} shinyTree you want to 
#' inspect.
#' @param format In which format you want the output. Use 
#' \code{names} to get a simple list of the names (with attributes
#' describing the node's ancestry), or \code{slices} to get a list
#' of lists, each of which is a slice of the list used to get down
#' to the selected node. 
#' @param leaves_only If \code{TRUE}, return only selected nodes that have no
#' children, leaving out selected parents. This works however deep each branch
#' goes, unlike filtering \code{slices} by depth.
#' @examples
#' tree <- list(
#'   World = structure(list(
#'     Europe = structure(list(
#'       Scandinavia = structure(list(
#'         Denmark = structure(0, stselected = TRUE),
#'         Sweden = structure(0, stselected = TRUE)
#'       ), stselected = TRUE),
#'       France = structure(0, stselected = TRUE)
#'     ), stselected = TRUE)
#'   ), stselected = TRUE)
#' )
#' unlist(get_selected(tree))
#' unlist(get_selected(tree, leaves_only = TRUE))
#' @export
get_selected <- function(tree, format=c("names", "slices", "classid"),
                         leaves_only = FALSE){
  format <- match.arg(format, c("names", "slices", "classid"), FALSE)
  make <- switch(format,
                 "names"=node_name,
                 "slices"=node_slice,
                 "classid"=node_classid)
  collect_nodes(tree, "stselected", make, leaves_only = leaves_only)
}

get_selected_names <- function(tree, ancestry=NULL, vec=list()){
  c(vec, collect_nodes(tree, "stselected", node_name, ancestry))
}

get_selected_slices <- function(tree, ancestry=NULL, vec=list()){
  c(vec, collect_nodes(tree, "stselected", node_slice, ancestry))
}

get_selected_classid <- function(tree, ancestry=NULL, vec=list()){
  c(vec, collect_nodes(tree, "stselected", node_classid, ancestry))
}

#' Collect the nodes of a tree that carry a given flag
#'
#' Walks \code{tree} depth-first and returns \code{make(node, ancestry)} for
#' each node whose \code{flag} attribute is \code{TRUE}, in the order a node's
#' descendants come before the node itself. Shared by the
#' \code{get_selected()} and \code{get_checked()} formats.
#'
#' Each subtree returns its own matches and they are joined once per level, so
#' the work is proportional to the number of nodes times the depth of the tree.
#' Appending to one growing result with \code{vec[length(vec)+1] <-} copied it
#' on every match instead, which made reading a large tree quadratic (#83).
#'
#' @param tree the \code{input$tree} list
#' @param flag attribute name to test, e.g. \code{"stselected"}
#' @param make function of \code{(node, ancestry)} building one result entry
#' @param ancestry names from the root down to \code{tree}
#' @param leaves_only if \code{TRUE}, skip nodes that have children. In
#' \code{input$tree} a node with children is a list and a leaf is not.
#' @noRd
collect_nodes <- function(tree, flag, make, ancestry = NULL, leaves_only = FALSE) {
  found <- list()
  if (is.list(tree)) {
    nodeNames <- names(tree)
    found <- do.call(c, lapply(seq_along(tree), function(i) {
      collect_nodes(tree[[i]], flag, make, c(ancestry, nodeNames[i]), leaves_only)
    }))
    if (is.null(found)) {
      found <- list()
    }
  }
  if (leaves_only && is.list(tree)) {
    return(found)
  }
  if (isTRUE(attr(tree, flag, TRUE) == TRUE)) {
    found[length(found) + 1] <- list(make(tree, ancestry))
  }
  found
}

# The node's name, with its ancestry and its st* attributes.
node_name <- function(tree, ancestry) {
  len_anc <- length(ancestry)
  el <- ancestry[len_anc]
  attr(el, "ancestry") <- ancestry[1:len_anc-1]
  #save attributes that start with "st" (ShinyTree)
  attrs <- attributes(tree)
  for (attribute in names(attrs)) {
    if (grepl("^st", attribute)) {
      attr(el, attribute) <- attrs[[attribute]]
    }
  }
  el
}

# A nested list of names from the root down to the node.
node_slice <- function(tree, ancestry) {
  ancList <- 0
  for (i in length(ancestry):1){
    nl <- list()
    nl[ancestry[i]] <- list(ancList)
    ancList <- nl
  }
  ancList
}

# The node's name, with its stclass, stid and id.
node_classid <- function(tree, ancestry) {
  el <- ancestry[length(ancestry)]
  attr(el, "stclass") <- attr(tree, "stclass", TRUE)
  attr(el, "stid") <- attr(tree, "stid", TRUE)
  attr(el, "id") <- attr(tree, "id", TRUE)
  el
}
