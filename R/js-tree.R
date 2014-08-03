#' Create a Shiny Tree
#' @export
jsTree <- function(outputId, checkbox=FALSE){
  js <- ""
  tagList(
    singleton(tags$head(
      initResourcePaths(),
      tags$link(rel = 'stylesheet',
                type = 'text/css',
                href = 'shinyTree/jsTree-3.0.2/themes/default/style.min.css'),
      tags$script(src = 'shinyTree/jsTree-3.0.2/jstree.min.js'),
      tags$script(src = 'shinyTree/shinyTree.js')
    )),
    div(id=outputId, class="shiny-tree", `data-checkbox`=checkbox),
    tags$script(type="text/javascript", HTML(js))
  )
}