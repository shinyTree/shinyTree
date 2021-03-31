#' Tree traversal
#'
#' Traverse through tree/list to set node attributes, e.g. change icons.
#' Useful for directory structure icons where inner nodes are directories, leafs are files.
#' 
#' @param tree named nested list
#' @param attr_name name of attribute to set
#' @param inner_val value of attribute for inner tree nodes
#' @param leaf_val value of attribute for outer tree nodes
#' @examples
#' tree <- dfToTree(data.frame(Titanic),  c("Sex", "Survived"))
#' str(set_node_attrs(tree, attr_name = "sttype", inner_val = "directory", leaf_val = "file"))
#'
#' @return named nested list
#' @export
set_node_attrs <- function(tree, attr_name, inner_val, leaf_val){
  if(length(tree) == 0 || is.atomic(tree)){ # matches leaves like "" or list() (which comes from dfToTree)
    set_attr(tree, attr_name, leaf_val)
  }else{
    res <- lapply(tree, set_node_attrs, attr_name, inner_val, leaf_val)
    set_attr(res, attr_name, inner_val)
  }
}

#' Set attribute on object
#'
#  Set attribute on object and return the object
#' @param obj any object
#' @param attr_name name of attribute to set
#' @param attr_value value of attribute to set
#' @return the same object with set attribute
set_attr <- function(obj, attr_name, attr_value){
  # set attribute on object and return the object
  attr(obj, attr_name) <- attr_value
  obj
}
