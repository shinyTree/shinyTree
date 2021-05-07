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
  if(length(tree) == 0 || is.atomic(tree)){
    # base case: leaf node is list() or ""
    attr(tree, attr_name) <- leaf_val
    tree
  }else{
    res <- lapply(tree, set_node_attrs, attr_name, inner_val, leaf_val)
    attr(res, attr_name) <- inner_val
    # Previous attributes are lost here, so we need to append them again
    attributes(res) <- append(attributes(tree), attributes(res))
    res
  }
}
