listToTags <- function(myList, parent=tags$ul()){
  
  # There's probably an *apply way to do this. Whatevs.
  for (i in 1:length(myList)){
    name <- names(myList)[i]
    if (is.null(name)){
      name <- as.character(myList[[i]])
    }
  
    attrib <- NULL
    
    # Handle 'opened' attribute
    opened <- attr(myList[[i]], "jtopened")
    if (!is.null(opened) && opened){
      attrib <- c(attrib, "\"opened\": true")
    }
    
    # Handle 'selected' attribute
    selected <- attr(myList[[i]], "jtselected")
    if (!is.null(selected) && selected){
      attrib <- c(attrib, "\"selected\": true")
    }
    
    # Handle 'disabled' attribute
    disabled <- attr(myList[[i]], "jtdisabled")
    if (!is.null(disabled) && disabled){
      attrib <- c(attrib, "\"disabled\": true")
    }
    
    if (is.list(myList[[i]])){
      parent <- tagAppendChild(parent, tags$li(name, listToTags(myList[[i]]), 
                                               `data-jstree`=paste0("{",paste(attrib, collapse = ","),"}")))
    } else{
      parent <- tagAppendChild(parent, tags$li(name, `data-jstree`=paste0("{",paste(attrib, collapse = ","),"}")))
    }
  }
  return(parent)
}