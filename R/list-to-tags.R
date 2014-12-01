listToTags <- function(myList, parent=shiny::tags$ul()){
  
  # Handle parent tag attributes
  el <- list(parent)
  if (!is.null(attr(myList, "stclass"))){
    el[["class"]] <- attr(myList, "stclass")
  }  
  attribJSON <- getJSON(myList)  
  if (!is.null(attribJSON)){
    el[["data-jstree"]] <- attribJSON
  }
  parent <- do.call(shiny::tagAppendAttributes, el)
  
  # There's probably an *apply way to do this. Whatevs.
  for (i in 1:length(myList)){
    name <- names(myList)[i]
    if (is.null(name)){
      name <- ""
    }
  
    attribJSON <- getJSON(myList[[i]])
    
    if (is.list(myList[[i]])){
      el <- list(name, listToTags(myList[[i]]), 
                 `data-jstree`=attribJSON)
      if (!is.null(attr(myList[[i]], "stclass"))){
        el[["class"]] <- attr(myList[[i]], "stclass")
      }
      parent <- shiny::tagAppendChild(parent, do.call(shiny::tags$li, el))
    } else{
      el <- list(name, `data-jstree`=attribJSON)
      if (!is.null(attr(myList[[i]], "stclass"))){
        el[["class"]] <- attr(myList[[i]], "stclass")
      }
      parent <- shiny::tagAppendChild(parent, do.call(shiny::tags$li, el))
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