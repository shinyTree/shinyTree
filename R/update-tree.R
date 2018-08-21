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


#' @importFrom jsonlite toJSON
Rlist2json <- function(nestedList) {
  json <- as.character(toJSON(get_flatList(nestedList), auto_unbox = T))
}

get_flatList <- function(nestedList, flatList = NULL, parent = "#") {
  for (name in names(nestedList)) {
    additionalAttributeNames <- c("icon","type")
    additionalAttributes<- lapply(additionalAttributeNames,function(attribute){
      if(attribute == "icon"){
        fixIconName(attr(nestedList[[name]],paste0("st",attribute)))
      }else{
        attr(nestedList[[name]],paste0("st",attribute))
      }
    })
    names(additionalAttributes) <-  additionalAttributeNames
    additionalAttributes <- additionalAttributes[which(sapply(additionalAttributes,Negate(is.null)))]

    nodeData <- append(
      list(
        id = as.character(length(flatList) + 1),
        text = name,
        parent = parent,
        state = list(
          opened   = isTRUE(attr(nestedList[[name]], "stopened")),
          selected = isTRUE(attr(nestedList[[name]], "stselected"))
        )
      ),
      additionalAttributes
    )

    flatList = c(flatList,list(nodeData))
    if (is.list(nestedList[[name]]))
      flatList =
        get_flatList(nestedList[[name]], flatList, parent = as.character(length(flatList)))
  }
  flatList
}
