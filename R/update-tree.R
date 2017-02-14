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



Rlist2json <- function(nestedList) {
  json <- as.character(jsonlite::toJSON(get_flatList(nestedList), auto_unbox = T))
}

get_flatList <- function(nestedList, flatList = NULL, parent = "#") {
  for (name in names(nestedList)) {
    flatList = c(flatList,
                 list(
                   list(
                     id = as.character(length(flatList) + 1),
                     text = name,
                     parent = parent,
                     state = list(
                       opened   = isTRUE(attr(nestedList[[name]], "stopened")),
                       selected = isTRUE(attr(nestedList[[name]], "stselected"))
                     )
                   )
                 ))
    if (is.list(nestedList[[name]]))
      flatList =
        get_flatList(nestedList[[name]], flatList, parent = as.character(length(flatList)))
  }
  flatList
}



