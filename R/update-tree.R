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
  ## This if-condition could be omitted, since message will never be NULL, also if data is NULL
  ## Or we test for is.null(data)
  # if(!is.null(message)) {
    session$sendInputMessage(treeId, message)
  # }
}


#' @importFrom jsonlite toJSON
Rlist2json <- function(nestedList) {
  as.character(toJSON(get_flatList(nestedList), auto_unbox = T))
}


#fix icon retains backward compatibility for icon entries that are not fully specified
fixIconName <- function(icon){
  if (is.null(icon)) {
    NULL
  } else if (grepl("[/\\]",icon)) { #ie. "/images/ball.jpg"
    icon
  } else{
    iconGroup <-  grep(pattern = "(\\S+) \\1-", x = icon, value=TRUE) #ie "fa fa-file"
    if (length(iconGroup) > 0L) {
      icon
    } else {
      icoGr = regmatches(x = icon, regexpr("(\\S+)-", icon))
      if (length(icoGr)==0) {
        icoGr = "fa fa-"
      } else if (icoGr == "fa-") {
        icoGr = "fa "
      } else if (icoGr == "glyphicon-") {
        icoGr = "glyphicon "
      } else {
        icoGr = "fa fa-"
      }
      if (!is.na(icoGr)) {
        paste0(icoGr, icon)
      } else { #ie. just "file"
        paste0("fa fa-", icon)
      }
      
    }
  }
}

# TODO - This recursive function could be written in C/C++ to speed things up (Especially if tree gets large)
# can fixIconName be included in C function then?
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


# get_flatList <- function(nestedList, flatList = NULL, parent = "#") {
#   for (name in names(nestedList)) {
#     additionalAttributeNames <- c("icon","type")
#     additionalAttributes<- lapply(additionalAttributeNames,function(attribute){
#       if(attribute == "icon"){
#         fixIconName(attr(nestedList[[name]],paste0("st",attribute)))
#       }else{
#         attr(nestedList[[name]],paste0("st",attribute))
#       }
#     })
#     names(additionalAttributes) <-  additionalAttributeNames
#     additionalAttributes <- additionalAttributes[which(sapply(additionalAttributes,Negate(is.null)))]
# 
#     nodeData <- append(
#       list(
#         id = as.character(length(flatList) + 1),
#         text = name,
#         parent = parent,
#         state = list(
#           opened   = isTRUE(attr(nestedList[[name]], "stopened")),
#           selected = isTRUE(attr(nestedList[[name]], "stselected"))
#         )
#       ),
#       additionalAttributes
#     )
# 
#     flatList = c(flatList,list(nodeData))
#     if (is.list(nestedList[[name]]))
#       flatList =
#         get_flatList(nestedList[[name]], flatList, parent = as.character(length(flatList)))
#   }
#   flatList
# }
