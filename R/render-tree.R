#' Render a Tree
#' @export
renderTree <- function(expr, env = parent.frame(), quoted = FALSE){
  func <- exprToFunction(expr, env, quoted)
  return(function(shinysession, name, ...) {
    tree <- func()
    
    HTML(as.character(listToTags(tree)))
  })
}