listToTags <- function(myList, parent=tags$ul()){
  
  # There's probably an *apply way to do this. Whatevs.
  for (i in 1:length(myList)){
    name <- names(myList)[i]
    if (is.null(name)){
      name <- ""
    }
  
    attrib <- NULL
    
    # Handle 'opened' attribute
    opened <- attr(myList[[i]], "stopened")
    if (!is.null(opened) && opened){
      attrib <- c(attrib, "\"opened\": true")
    }
    
    # Handle 'selected' attribute
    selected <- attr(myList[[i]], "stselected")
    if (!is.null(selected) && selected){
      attrib <- c(attrib, "\"selected\": true")
    }
    
    # Handle 'disabled' attribute
    disabled <- attr(myList[[i]], "stdisabled")
    if (!is.null(disabled) && disabled){
      attrib <- c(attrib, "\"disabled\": true")
    }
    
    # Handle 'icon' attribute
    icon <- attr(myList[[i]], "sticon")
    if (!is.null(icon)){
      icon <- paste0("fa fa-",icon)
      attrib <- c(attrib, paste0("\"icon\": \"", icon, "\""))
    }
    
    attribJSON <- paste0("{",paste(attrib, collapse = ","),"}")
    
    if (is.list(myList[[i]])){
      parent <- tagAppendChild(parent, tags$li(name, listToTags(myList[[i]]), 
                                               `data-jstree`=attribJSON))
    } else{
      parent <- tagAppendChild(parent, tags$li(name, `data-jstree`=attribJSON))
    }
  }
  return(parent)
}