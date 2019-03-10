.global <- new.env()

initResourcePaths <- function() {
  if (is.null(.global$loaded)) {
    shiny::addResourcePath(
      prefix = 'shinyTree',
      directoryPath = system.file('www', package='shinyTree'))
    .global$loaded <- TRUE
  }
  shiny::HTML("")
}

# Parse incoming shinyTree input from the client
#' @importFrom methods loadMethod
.onAttach <- function(libname, pkgname){
  shiny::registerInputHandler("shinyTree", function(val, shinysession, name){
    #The callbackCounter exists to make sure shiny gets an update after this sequence:
    #1. The user changes the tree
    #2. The R server restores the tree back to the previous version (because of logic that prevents the user change)
    #3. The user tries to make the same change.
    #Because the tree would otherwise send the same json message twice, shiny blocks the message. By havng an incrementing
    #callbackCounter, the app is assured to receive the message
    val$callbackCounter <- NULL #set to null to avoid problems with jsonToAttr
    jsonToAttr(val)    
  })
}

jsonToAttr <- function(json){
  ret <- list()
  
  if (!"text" %in% names(json)) {
    # This is a top-level list, not a node.
    for (i in 1:length(json)) {
      ret[[json[[i]]$text]] <- jsonToAttr(json[[i]])
      ret[[json[[i]]$text]] <- supplementAttr(ret[[json[[i]]$text]], json[[i]])
    }
    return(ret)
  }
  
  if (length(json$children) > 0) {
    return(jsonToAttr(json[["children"]]))
  } else {
    ret <- 0
    ret <- supplementAttr(ret, json)    
    return(ret)
  }
}

supplementAttr <- function(ret, json){
  if (json$state$selected != FALSE) {
    ## If checkbox is TRUE and a new node is created, emits error if this if-test is not included
    # if (!is.null(attr(ret, "stselected"))) {
      attr(ret, "stselected") <- json$state$selected
    # }
  }
  if (json$state$disabled != FALSE) {
    # if (!is.null(attr(ret, "disabled"))) {
      attr(ret, "stdisabled") <- json$state$disabled
    # }
  }
  if (json$state$opened != FALSE) {
    # if (!is.null(attr(ret, "opened"))) {
      attr(ret, "stopened") <- json$state$opened
    # }
  }
  if (exists('id', where = json)) {
    ## This happens when a new node is created on a parent with no children.
    ## Or a new node is created on a parent that is closed.
    if (!is.null(attr(ret, "id"))) {
      attr(ret, "id") <- json$id
    }
  }
  ret
}
