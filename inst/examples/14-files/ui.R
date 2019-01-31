library(shiny)
library(shinyTree)

#' @author Michael Bell \email{bellma@@lilly.com}
shinyUI(
  pageWithSidebar(
    # Application title
    headerPanel("Simple shinyTree!"),
    
    sidebarPanel(
      helpText(HTML("Directory Structure Example."))
    ),
    mainPanel(
      # Show a simple table.
      shinyTree("tree",types= #Types is in the same format that jstree expects
        "{
          'directory' : { 'icon' : 'glyphicon glyphicon-folder-open' },
          'default' : { 'icon' : 'glyphicon glyphicon-file', 'valid_children' : [] }
         }"
      )
    )
  ))