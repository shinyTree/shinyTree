library(shiny)
library(shinyTree)

#' @author Sandro Raabe \email{sa.ra.online@posteo.de}
shinyUI(
  pageWithSidebar(
    # Application title
    headerPanel("shinyTree Directory structure"),
    
    sidebarPanel(
      helpText(HTML("Recursively change icons of tree nodes."))
    ),
    mainPanel(
      # Show a simple table.
      shinyTree("tree",types=
        "{
          'directory' : { 'icon' : 'glyphicon glyphicon-folder-open' },
          'file' : { 'icon' : 'glyphicon glyphicon-file', 'valid_children' : [] }
         }"
      )
    )
  ))