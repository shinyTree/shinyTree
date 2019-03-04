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
  as.character(toJSON(get_flatList(nestedList), auto_unbox = T))
}

#' @importFrom stringr str_subset str_match
#fix icon retains backward compatibility for icon entries that are not fully specified
fixIconName <- function(icon){
  if(is.null(icon)){
    NULL
  }else if(grepl("[/\\]",icon)){ #ie. "/images/ball.jpg"
    icon
  }else{
    iconGroup <- str_subset(icon,"(\\S+) \\1-") #ie "fa fa-file"
    if(length(iconGroup) > 0){
      icon
    }else{
      iconGroup <- str_match(icon,"(fa|glyphicon)-") #ie "fa-file"
      if(length(iconGroup) > 1 && !is.na(iconGroup[2])){
        paste(iconGroup[2],icon)
      }else{ #ie. just "file"
        paste0("fa fa-",icon)
      }
    }
  }
}

get_flatList <- function(nestedList, flatList = NULL, parent = "#") {
  for (name in names(nestedList)) {
    additionalAttributes <- list(
      "icon" = fixIconName(attr(nestedList[[name]],"sticon")),
      "type" = attr(nestedList[[name]],"sttype")
    )
    additionalAttributes <- additionalAttributes[which(sapply(additionalAttributes,Negate(is.null)))]
    
    data <- lapply(names(attributes(nestedList[[name]])),function(key){
      if(key %in% c("icon","type","names","stopened","stselected","sttype")){
        NULL
      }else{
        attr(nestedList[[name]],key)
      }
    })
    if(!is.null(data) && length(data) > 0){
      names(data) <- names(attributes(nestedList[[name]]))
      data <- data[which(sapply(data,Negate(is.null)))]
    }
    
    nodeData <- append(
      list(
        id = as.character(length(flatList) + 1),
        text = name,
        parent = parent,
        state = list(
          opened   = isTRUE(attr(nestedList[[name]], "stopened")),
          selected = isTRUE(attr(nestedList[[name]], "stselected"))
        ),
        data = data
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
