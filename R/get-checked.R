#' Get the checked nodes from a tree
#' 
#' Extract the nodes from the tree that are checked in a more
#' convenient format. You can choose which format you prefer.
#' 
#' @param tree The \code{input$tree} shinyTree you want to 
#' inspect.
#' @param format In which format you want the output. Use 
#' \code{names} to get a simple list of the names (with attributes
#' describing the node's ancestry), or \code{slices} to get a list
#' of lists, each of which is a slice of the list used to get down
#' to the checked node. \code{classid} gives each name with its
#' \code{stclass}, \code{stid} and \code{id} (jsTree's id for the node)
#' attributes instead of its ancestry.
#' @param leaves_only If \code{TRUE}, return only checked nodes that have no
#' children, leaving out checked parents. With \code{three_state = TRUE} a
#' parent is checked whenever all of its children are, so this gives just the
#' items that were ticked.
#' @examples
#' tree <- list(
#'   Fruit = structure(list(
#'     Apple = structure(0, stchecked = TRUE),
#'     Pear = 0
#'   ), stopened = TRUE)
#' )
#' unlist(get_checked(tree))
#' @return A list with one element per checked node, descendants before their
#'   ancestors, in the form \code{format} gives. An empty list if nothing is
#'   checked or \code{tree} is \code{NULL}.
#' @seealso \code{\link{get_selected}}
#' @export
get_checked <- function(tree, format=c("names", "slices", "classid"),
                        leaves_only = FALSE){
  format <- match.arg(format, c("names", "slices", "classid"), FALSE)
  make <- switch(format,
                 "names"=node_name,
                 "slices"=node_slice,
                 "classid"=node_classid)
  collect_nodes(tree, "stchecked", make, leaves_only = leaves_only)
}

get_checked_names <- function(tree, ancestry=NULL, vec=list()){
  c(vec, collect_nodes(tree, "stchecked", node_name, ancestry))
}

get_checked_slices <- function(tree, ancestry=NULL, vec=list()){
  c(vec, collect_nodes(tree, "stchecked", node_slice, ancestry))
}

get_checked_classid <- function(tree, ancestry=NULL, vec=list()){
  c(vec, collect_nodes(tree, "stchecked", node_classid, ancestry))
}
