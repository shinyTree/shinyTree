listToTags <- function(myList, parent=tags$ul()){
  
  # Handle parent tag attributes
  attribJSON <- getJSON(myList)
  parent <- tagAppendAttributes(parent, `data-jstree`=attribJSON)
  
  # There's probably an *apply way to do this. Whatevs.
  for (i in 1:length(myList)){
    name <- names(myList)[i]
    if (is.null(name)){
      name <- ""
    }
  
    attribJSON <- getJSON(myList[[i]])
    
    if (is.list(myList[[i]])){
      parent <- tagAppendChild(parent, tags$li(name, listToTags(myList[[i]]), 
                                               `data-jstree`=attribJSON))
    } else{
      parent <- tagAppendChild(parent, tags$li(name, `data-jstree`=attribJSON))
    }
  }
  return(parent)
}

getJSON <- function(node){
  attrib <- NULL
  
  # Handle 'opened' attribute
  opened <- attr(node, "stopened")
  if (!is.null(opened) && opened){
    attrib <- c(attrib, "\"opened\": true")
  }
  
  # Handle 'selected' attribute
  selected <- attr(node, "stselected")
  if (!is.null(selected) && selected){
    attrib <- c(attrib, "\"selected\": true")
  }
  
  # Handle 'disabled' attribute
  disabled <- attr(node, "stdisabled")
  if (!is.null(disabled) && disabled){
    attrib <- c(attrib, "\"disabled\": true")
  }
  
  # Handle 'icon' attribute
  icon <- attr(node, "sticon")
  if (!is.null(icon)){
    icon <- paste0("fa fa-",icon)
    attrib <- c(attrib, paste0("\"icon\": \"", icon, "\""))
  }
  
  paste0("{",paste(attrib, collapse = ","),"}")  
}