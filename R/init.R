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
.onLoad <- function(libname, pkgname) {
  ## set the default parser to list to keep backwards compatibility
  ## can be set by the user to "tree" to receive a data.tree
  options(shinyTree.defaultParser = "list")
  
  shiny::registerInputHandler("shinyTree", function(val, shinysession, name){
    #The callbackCounter exists to make sure shiny gets an update after this sequence:
    #1. The user changes the tree
    #2. The R server restores the tree back to the previous version (because of logic that prevents the user change)
    #3. The user tries to make the same change.
    #Because the tree would otherwise send the same json message twice, shiny blocks the message. By having an incrementing
    #callbackCounter, the app is assured to receive the message
    val$callbackCounter <- NULL #set to null to avoid problems with jsonToAttr
    
    ## use the parser specified by option shinyTree.defaultParser
    ## if library data.tree is not installed use list parsing nevertheless
    ## make sure that parser is either 'list' or 'tree'
    defaultParser <- getOption("shinyTree.defaultParser")
    
    if (defaultParser == "tree" && !requireNamespace("data.tree", quietly = TRUE)) {
      ## use paste to avoid too long lines
      warning(paste("library 'data.tree' cannot be loaded, falling back to list parser",
                    "(check 'options(\"shinyTree.defaultParser\")')"),
              domain = NA)
      defaultParser <- "list"
    }
    
    if (defaultParser == "list") {
      jsonToAttr(val)      
    } else if (defaultParser == "tree") {
      data.tree::as.Node(list(children = val),
                         "explicit",
                         nameName = "text")
    } else {
      ## unknown parser
      msg <- sprintf(paste("unknown parser %s, falling back to list parser",
                           "(check 'options(\"shinyTree.defaultParser\")')"),
                     dQuote(defaultParser))
      warning(msg,
              domain = NA)
      jsonToAttr(val)
    }
  })
}

jsonToAttr <- function(json){
  ret <- list()
  
  if (! "text" %in% names(json)){
    # This is a top-level list, not a node.
    for (i in 1:length(json)){
      ret[[json[[i]]$text]] <- jsonToAttr(json[[i]])
      ret[[json[[i]]$text]] <- supplementAttr(ret[[json[[i]]$text]], json[[i]])
    }
    return(ret)
  }
  
  if (length(json$children) > 0){
    return(jsonToAttr(json[["children"]]))
  } else {
    ret <- 0
    ret <- supplementAttr(ret, json)    
    return(ret)
  }
}

supplementAttr <- function(ret, json){
  # Only add attributes if non-default
  if(!is.null(ret)){
    sapply(names(json$data),function(name){
      attr(ret, name) <<- json$data[[name]]
    })
    
    if (json$state$selected != FALSE){
      attr(ret, "stselected") <- json$state$selected
    }
    if (json$state$checked != FALSE){
      attr(ret, "stchecked") <- json$state$checked
    }
    if (json$state$disabled != FALSE){
      attr(ret, "stdisabled") <- json$state$disabled
    }
    if (json$state$opened != FALSE){
      attr(ret, "stopened") <- json$state$opened
    }
    if (exists('id', where=json)) {
      attr(ret, "id") <- json$id
    }
  }
  ret
}
