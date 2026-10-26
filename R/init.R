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

.onUnload <- function(libpath) {
  shiny::removeInputHandler("shinyTree")
}

# Parse incoming shinyTree input from the client
.onLoad <- function(libname, pkgname) {
  ## set the default parser to list to keep backwards compatibility
  ## can be set by the user to "tree" to receive a data.tree -- including
  ## before the package is loaded, e.g. in .Rprofile, so a value already set
  ## is kept
  if (is.null(getOption("shinyTree.defaultParser"))) {
    options(shinyTree.defaultParser = "list")
  }
  
  ## force = TRUE so the package can be loaded again in the same session, as
  ## devtools::load_all() does, without failing on the handler it registered
  ## the first time
  shiny::registerInputHandler("shinyTree", force = TRUE, function(val, shinysession, name){
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
    defaultParser <- getOption("shinyTree.defaultParser", "list")
    if (!is.character(defaultParser) || length(defaultParser) != 1 || is.na(defaultParser)) {
      defaultParser <- "list"
    }
    
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
      warnDuplicateSiblings(val)
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
    # Assign positionally and name afterwards: keying by text would let
    # same-named siblings overwrite each other (#104).
    ret <- vector("list", length(json))
    for (i in seq_along(json)){
      ret[[i]] <- supplementAttr(jsonToAttr(json[[i]]), json[[i]])
    }
    names(ret) <- vapply(json, function(node) node$text, character(1))
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
    for (name in names(json$data)) {
      value <- json$data[[name]]
      ret <- tryCatch({
        attr(ret, name) <- value
        ret
      }, error = function(e) {
        # R gives some attribute names a meaning of its own and refuses values
        # that do not fit it. Trees built from XML or data.tree often carry a
        # field called "class" holding a list, which raised "attempt to set
        # invalid 'class' attribute" and stopped input$tree from being read at
        # all (#58). Keep such data under an "st" prefix instead -- "class"
        # becomes "stclass" -- unless the node already has data there. Values
        # R accepts are set as before.
        prefixed <- paste0("st", name)
        if (!prefixed %in% names(json$data)) {
          attr(ret, prefixed) <- value
        }
        ret
      })
    }

    # isTRUE() rather than `!= FALSE`: a state key may be missing entirely --
    # treeToJSON() emits no "checked" -- and `NULL != FALSE` is logical(0),
    # which makes if() raise "argument is of length zero" (#103).
    if (isTRUE(json$state$selected)){
      attr(ret, "stselected") <- json$state$selected
    }
    if (isTRUE(json$state$checked)){
      attr(ret, "stchecked") <- json$state$checked
    }
    if (isTRUE(json$state$disabled)){
      attr(ret, "stdisabled") <- json$state$disabled
    }
    if (isTRUE(json$state$opened)){
      attr(ret, "stopened") <- json$state$opened
    }
    if (!is.null(json[["id"]])) {
      attr(ret, "id") <- json[["id"]]
    }
  }
  ret
}

# data.tree keeps one child of each name, so siblings sharing a name -- which
# shinyTree allows (#104) -- would be merged silently by the "tree" parser.
warnDuplicateSiblings <- function(nodes) {
  names <- vapply(nodes, function(node) as.character(node$text %||% ""), character(1))
  if (anyDuplicated(names)) {
    warning("input$tree has siblings with the same name, which the \"tree\" parser ",
            "(options(shinyTree.defaultParser = \"tree\")) cannot keep apart: ",
            paste(sQuote(unique(names[duplicated(names)])), collapse = ", "),
            ". Use the \"list\" parser to receive them all.", call. = FALSE)
    return(invisible())
  }
  for (node in nodes) {
    if (length(node$children)) warnDuplicateSiblings(node$children)
  }
  invisible()
}
