#' Create a Shiny Tree
#' 
#' This creates a spot in your Shiny UI for a shinyTree which can then be filled
#' in using \code{\link{renderTree}}.
#' 
#' A shinyTree is an output *and* an input element in the same time. While you can 
#' fill it via \code{\link{renderTree}} you can access its content via \code{input$tree} 
#' (for example after the user rearranged some nodes). By default, \code{input$tree} will
#' return a list similiar to the one you use to fill the tree. This behaviour is controlled
#' by \code{getOption("shinyTree.defaultParser")}. It defaults to \code{"list"}, but can be set 
#' to \code{"tree"}, in which case a \code{\link[data.tree]{data.tree}} is returned.
#' 
#' @param outputId The ID associated with this element
#' @param checkbox If \code{TRUE}, will enable checkboxes next to each node to 
#' make the selection of multiple nodes in the tree easier.
#' @param searchplaceholder Add a placeholder value to the search box
#' @param search If \code{TRUE}, will enable search functionality in the tree by adding
#' a search box above the produced tree. Alternatively, you can set the parameter
#' to the ID of the text input you wish to use as the search field.
#' @param searchtime Determines the reaction time of the search algorithm.
#' Default is 250ms.
#' @param dragAndDrop If \code{TRUE}, will allow the user to rearrange the nodes in the
#' tree.
#' @param types enables jstree types functionality when sent proper json (please see the types example)
#' @param theme jsTree theme, one of \code{default}, \code{default-dark}, or \code{proton}.
#' @param themeIcons If \code{TRUE}, will show theme icons for each item.
#' @param themeDots If \code{TRUE}, will include level dots.
#' @param sort If \code{TRUE}, will sort the nodes in alphabetical/numerical
#' order.
#' @param unique If \code{TRUE}, will ensure that no node name exists more
#' than once.
#' @param wholerow If \code{TRUE}, will highlight the whole selected row.
#' @param stripes If \code{TRUE}, the tree background is striped.
#' @param multiple If \code{TRUE}, multiple nodes can be selected.
#' @param three_state If \code{TRUE}, checking a node cascades to its children,
#' and a parent is shown checked only while all of its children are (and as
#' undetermined while some are).
#' @param whole_node If \code{TRUE}, clicking anywhere on a node toggles its
#' checkbox, as if the box itself were clicked. If \code{FALSE}, only the box
#' does; clicking the label behaves as it would without checkboxes.
#' @param tie_selection If \code{TRUE}, checked and selected are the same thing:
#' ticking a box selects the node and selecting a node ticks its box, so
#' \code{\link{get_checked}} and \code{\link{get_selected}} report the same
#' nodes. If \code{FALSE}, checking is kept separate from the selection, so a
#' node can be checked without being selected. With \code{whole_node = TRUE}
#' as well, every click toggles a checkbox and nothing can be selected by
#' clicking.
#' @param animation The open / close animation duration in milliseconds.
#' Set this to \code{FALSE} to disable the animation (default is 200).
#' @param contextmenu If \code{TRUE}, will enable a contextmenu to
#' create/rename/delete/cut/copy/paste nodes.
#' @section Checked and selected nodes:
#' Without checkboxes, clicking a node selects it and
#' \code{\link{get_selected}} reports the selection. With
#' \code{checkbox = TRUE}, \code{tie_selection} decides whether a checked
#' node counts as selected; see its description above. The
#' \code{04-selected} example shows \code{get_selected} and
#' \code{get_checked} side by side and can switch between every combination
#' of \code{tie_selection}, \code{whole_node} and \code{three_state}.
#'
#' @section Options:
#' Two options control whether additional jsTree events update \code{input$tree}.
#' Both default to \code{FALSE}:
#' \describe{
#'   \item{\code{shinyTree.refresh}}{report \code{refresh.jstree} events.}
#'   \item{\code{shinyTree.setState}}{report \code{set_state.jstree} events.}
#' }
#' A refresh triggered by \code{\link{updateTree}} is never reported, since the
#' server already knows about a change it made itself; reporting it back caused
#' a feedback loop in apps that call \code{updateTree} from an observer watching
#' \code{input$tree}. Refreshes from any other source are still reported when
#' the option is enabled.
#' @seealso \code{\link{renderTree}}
#' @export
shinyTree <- function(outputId, checkbox=FALSE, search=FALSE, 
                      searchtime = 250, searchplaceholder = "", dragAndDrop=FALSE, types=NULL, 
                      theme="default", themeIcons=TRUE, themeDots=TRUE,
                      sort=FALSE, unique=FALSE, wholerow=FALSE,
                      stripes=FALSE, multiple=TRUE, animation=200,
                      contextmenu=FALSE, three_state=TRUE, whole_node=TRUE, tie_selection=TRUE){
  
  if ((!is.null(contextmenu) && contextmenu) && (!is.null(checkbox) && checkbox)) {
    warning("The plugins contextmenu and checkbox cannot be used together. \nSet checkbox to FALSE")
    checkbox = FALSE
  }
  
  searchEl <- shiny::div("")
  if (search == TRUE){
    search <- paste0(outputId, "-search-input")
    searchEl <- shiny::tags$input(id=search, class="input", type="text", value="", placeholder = searchplaceholder)
  }
  if (is.character(search)){
    # Either the search field we just created or the given text field ID
    searchEl <- shiny::tagAppendChild(searchEl, shiny::tags$script(type="text/javascript", shiny::HTML(
      paste0("shinyTree.initSearch('",outputId,"','",search,"', ", searchtime,");"))))
  }
  
  if(!theme %in% c("default","default-dark","proton")) { stop(paste("shinyTree theme, ",theme,", doesn't exist!",sep="")) }
  
  opts <- jsonlite::toJSON(list(
    'setState' = getOption('shinyTree.setState'),
    'refresh' = getOption('shinyTree.refresh')
    ), auto_unbox = T)
  
  if(!animation){
    animation = 'false'
  }
  if(!is.null(types)){
    outputnohyp <- gsub("-","_",outputId, fixed=T)
    types <- paste0(outputnohyp,"_sttypes = ",types)
  }
  shiny::tagList(
    # The scripts and stylesheets are declared as dependencies rather than
    # written into <head>, so they load wherever the tree is rendered: R
    # Markdown documents with runtime: shiny drop hand-written <head> tags, which
    # left the tree empty there (#18). Shiny includes each dependency once per
    # page however many trees use it.
    shinyTreeDependencies(theme),
    initResourcePaths(),
    if (!is.null(types)) shiny::tags$script(shiny::HTML(types)),
    searchEl,
    shiny::div(id=outputId, class="shiny-tree", 
        `data-st-checkbox`=checkbox, 
        `data-st-tie-selection`=tie_selection,
        `data-st-whole-node`=whole_node,
        `data-st-three-state`=three_state,
        `data-st-search`=is.character(search),
        `data-st-dnd`=dragAndDrop,
        `data-st-types`=!is.null(types),
        `data-st-theme`=theme,
        `data-st-theme-icons`=themeIcons,
        `data-st-theme-dots`=themeDots,
        `data-st-theme-stripes`=stripes,
        `data-st-multiple`=multiple,
        `data-st-animation`=animation,
        `data-st-sort`=sort,
        `data-st-unique`=unique,
        `data-st-wholerow`=wholerow,
        `data-st-contextmenu`=contextmenu,
        options = opts
        )
  )
}

#' The scripts and stylesheets a shinyTree needs, as htmlDependency objects
#'
#' One dependency per theme, so a page with trees in several themes loads each
#' theme stylesheet once. shinyTree.css comes after the theme stylesheet so it
#' can correct gaps in individual themes.
#' @noRd
shinyTreeDependencies <- function(theme) {
  version <- as.character(utils::packageVersion("shinyTree"))
  list(
    htmltools::htmlDependency(
      name = "jstree",
      version = "3.3.7",
      src = c(file = system.file("www/jsTree-3.3.7", package = "shinyTree")),
      script = "jstree.min.js"
    ),
    htmltools::htmlDependency(
      name = paste0("jstree-theme-", theme),
      version = "3.3.7",
      src = c(file = system.file(file.path("www/jsTree-3.3.7/themes", theme),
                                 package = "shinyTree")),
      stylesheet = "style.min.css"
    ),
    htmltools::htmlDependency(
      name = "shinyTree",
      version = version,
      src = c(file = system.file("www", package = "shinyTree")),
      script = "shinyTree.js",
      stylesheet = "shinyTree.css"
    ),
    htmltools::htmlDependency(
      name = "font-awesome-shinyTree",
      version = "7.0.0",
      src = c(href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/7.0.0"),
      stylesheet = list(list(
        href = "css/all.min.css",
        integrity = "sha512-DxV+EoADOkOygM4IR9yXP8Sb2qwgidEmeqAEmDKIOfPRQZOWbXCzLC6vjbZyy0vPisbH2SyW27+ddLVCN+OMzQ==",
        crossorigin = "anonymous"
      ))
    )
  )
}
