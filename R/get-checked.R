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
#' to the selected node. 
#' @export
get_checked<- function(tree, format=c("names", "slices", "classid")){
  format <- match.arg(format, c("names", "slices", "classid"), FALSE)
  switch(format,
         "names"=get_checked_names(tree),
         "slices"=get_checked_slices(tree),
         "classid"=get_checked_classid(tree)
         )  
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
