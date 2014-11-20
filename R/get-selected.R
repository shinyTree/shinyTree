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
#' @export
get_selected <- function(tree, format=c("names", "slices")){
  format <- match.arg(format, c("names", "slices"), FALSE)
  switch(format,
         "names"=get_selected_names(tree),
         "slices"=get_selected_slices(tree))  
}

get_selected_names <- function(tree, ancestry=NULL, vec=list()){
  if (is.list(tree)){
    for (i in 1:length(tree)){
      anc <- c(ancestry, names(tree)[i])
      vec <- get_selected_names(tree[[i]], anc, vec)
    }    
  }
  
  a <- attr(tree, "stselected", TRUE)
  if (!is.null(a) && a == TRUE){
    # Get the element name
    el <- tail(ancestry,n=1)
    vec[length(vec)+1] <- el
    attr(vec[[length(vec)]], "ancestry") <- head(ancestry, n=length(ancestry)-1)
  }
  return(vec)
}

get_selected_slices <- function(tree, ancestry=NULL, vec=list()){
  if (is.list(tree)){
    for (i in 1:length(tree)){
      anc <- c(ancestry, names(tree)[i])
      vec <- get_selected_slices(tree[[i]], anc, vec)
    }    
  }
  
  a <- attr(tree, "stselected", TRUE)
  if (!is.null(a) && a == TRUE){
    # Get the element name
    ancList <- 0
    
    for (i in length(ancestry):1){
      nl <- list()
      nl[ancestry[i]] <- list(ancList)
      ancList <- nl
    }
    vec[length(vec)+1] <- list(ancList)
  }
  return(vec)
}