.global <- new.env()

initResourcePaths <- function() {
  if (is.null(.global$loaded)) {
    shiny::addResourcePath(
      prefix = 'shinyTree',
      directoryPath = system.file('www', package='shinyTree'))
    .global$loaded <- TRUE
  }
  HTML("")
}

# Parse incoming shinyTable input from the client
.onLoad <- function(libname, pkgname){
  shiny::registerInputHandler("shinyTable", function(val, shinysession, name){
    val
    
  })
}

jsonToAttr <- function(json){
  ret <- list()
  
  if (! "text" %in% names(json)){
    # This is a top-level list, not a node.
    for (i in 1:length(json)){
      ret[[json[[i]]$text]] <- jsonToAttr(json[[i]])
    }
    return(ret)
  }
  
  if (length(json$children) > 0){
    return(jsonToAttr(json[["children"]]))
  } else {
    return("")
  }
}