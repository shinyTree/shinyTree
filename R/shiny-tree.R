#' Create a Shiny Tree
#' 
#' This creates a spot in your Shiny UI for a shinyTree which can then be filled
#' in using \code{\link{renderTree}}
#' @param outputId The ID associated with this element
#' @param selected The input ID associated with tree's currently selected 
#'   elements. The names of the node(s) that are currently selected in the tree 
#'   will be available in an input by this name.
#' @param checkbox If \code{TRUE}, will enable checkboxes next to each node to 
#'   make the selection of multiple nodes in the tree easier.
#' @param search If \code{TRUE}, will enable search functionality in the tree by adding
#'   a search box above the produced tree. Alternatively, you can set the parameter
#'   to the ID of the text input you wish to use as the search field.
#' @seealso \code{\link{renderTree}}
#' @export
shinyTree <- function(outputId, selected=NULL, checkbox=FALSE, search=FALSE){
  if (is.null(selected)) {
    selected <- ""
  } else {
    if (!is.character(selected)) {
      stop("`selected` must either be NULL or a character value.") 
    }
  }
  
  
  searchEl <- div("")
  if (search == TRUE){
    search <- paste0(outputId, "-search-input")
    searchEl <- tags$input(id=search, class="input", type="text", value="")
  }
  if (is.character(search)){
    # Either the search field we just created or the given text field ID
    searchEl <- tagAppendChild(searchEl, tags$script(type="text/javascript", HTML(
      paste0("shinyTree.initSearch('",outputId,"','",search,"');"))))
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
    searchEl,
    div(id=outputId, class="shiny-tree", 
        `data-st-checkbox`=checkbox, 
        `data-st-search`=is.character(search), 
        `data-st-selected`=selected)
  )
}