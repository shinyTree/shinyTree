#' Create a Shiny Tree
#' @param outputId The ID associated with this element
#' @param checkbox If \code{TRUE}, will enable checkboxes next to each node to 
#'   make the selection of multiple nodes in the tree easier.
#' @param selected The input ID associated with tree's currently selected
#'   elements. The names of the node(s) that are currently selected in the tree
#'   will be available in an input by this name.
#' @export
jsTree <- function(outputId, checkbox=FALSE, selected=NULL){
  if (is.null(selected)) {
    selected <- ""
  } else {
    if (!is.character(selected)) {
      stop("`selected` must either be NULL or a character value.") 
    }
  }
  
  tagList(
    singleton(tags$head(
      initResourcePaths(),
      tags$link(rel = 'stylesheet',
                type = 'text/css',
                href = 'shinyTree/jsTree-3.0.2/themes/default/style.min.css'),
      tags$link(rel = "stylesheet", 
                type = "text/css", 
                href = "shared/font-awesome/css/font-awesome.min.css"),
      tags$script(src = 'shinyTree/jsTree-3.0.2/jstree.min.js'),
      tags$script(src = 'shinyTree/shinyTree.js')
    )),
    div(id=outputId, class="shiny-tree", `data-st-checkbox`=checkbox, `data-st-selected`=selected)
  )
}