#' Create a Shiny Tree
#' 
#' This creates a spot in your Shiny UI for a shinyTree which can then be filled
#' in using \code{\link{renderTree}}
#' @param outputId The ID associated with this element
#' @param checkbox If \code{TRUE}, will enable checkboxes next to each node to 
#'   make the selection of multiple nodes in the tree easier.
#' @param search If \code{TRUE}, will enable search functionality in the tree by adding
#'   a search box above the produced tree. Alternatively, you can set the parameter
#'   to the ID of the text input you wish to use as the search field.
#' @param dragAndDrop If \code{TRUE}, will allow the user to rearrange the nodes in the
#'   tree.
#' @seealso \code{\link{renderTree}}
#' @export
shinyTree <- function(outputId, checkbox=FALSE, search=FALSE, dragAndDrop=FALSE){
  searchEl <- shiny::div("")
  if (search == TRUE){
    search <- paste0(outputId, "-search-input")
    searchEl <- shiny::tags$input(id=search, class="input", type="text", value="")
  }
  if (is.character(search)){
    # Either the search field we just created or the given text field ID
    searchEl <- shiny::tagAppendChild(searchEl, shiny::tags$script(type="text/javascript", shiny::HTML(
      paste0("shinyTree.initSearch('",outputId,"','",search,"');"))))
  }
  
  shiny::tagList(
    shiny::singleton(shiny::tags$head(
      initResourcePaths(),
      shiny::tags$link(rel = 'stylesheet',
                type = 'text/css',
                href = 'shinyTree/jsTree-3.0.2/themes/default/style.min.css'),
      shiny::tags$link(rel = "stylesheet", 
                type = "text/css", 
                href = "shared/font-awesome/css/font-awesome.min.css"),
      shiny::tags$script(src = 'shinyTree/jsTree-3.0.2/jstree.min.js'),
      shiny::tags$script(src = 'shinyTree/shinyTree.js')
    )),
    searchEl,
    shiny::div(id=outputId, class="shiny-tree", 
        `data-st-checkbox`=checkbox, 
        `data-st-search`=is.character(search),
        `data-st-dnd`=dragAndDrop)
  )
}