library(stringr)

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
      iconGroup <- str_match(icon,"(\\S+)-") #ie "fa-file"
      if(length(iconGroup) > 1 && !is.na(iconGroup[2])){
        paste(iconGroup[2],icon)
      }else{ #ie. just "file"
        paste0("fa fa-",icon)
      }
    }
  }
}

listToTags <- function(myList, parent=shiny::tags$ul()){
  
  # Handle parent tag attributes
  el <- list(parent)
  if (!is.null(attr(myList, "stid"))){
    el[["stid"]] <- attr(myList, "stid")
  }
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
      if (!is.null(attr(myList[[i]], "stid"))){
        el[["stid"]] <- attr(myList[[i]], "stid")
      }
      if (!is.null(attr(myList[[i]], "stclass"))){
        el[["class"]] <- attr(myList[[i]], "stclass")
      }
      parent <- shiny::tagAppendChild(parent, do.call(shiny::tags$li, el))
    } else{
      el <- list(name, `data-jstree`=attribJSON)
      if (!is.null(attr(myList[[i]], "stid"))){
        el[["stid"]] <- attr(myList[[i]], "stid")
      }
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
    attrib <- c(attrib, paste0("\"icon\": \"", fixIconName(icon), "\""))
  }

  # Handle 'type' attribute
  type <- attr(node, "sttype")
  if (!is.null(type)){
    attrib <- c(attrib, paste0("\"type\": \"", type, "\""))
  }
  
  paste0("{",paste(attrib, collapse = ","),"}")  
}
