library(shiny)
library(shinyTree)

#' Demonstrates icons by using types and by paticular items in the tree
#' @author Michael Bell \email{bellma@@lilly.com}
shinyUI(
  pageWithSidebar(
    # Application title
    headerPanel("shinyTree with icons!"),
    
    sidebarPanel(
      helpText(HTML("A shinyTree example for displaying various icons using types and specifying particular items in the tree
                  <hr>Created using shinyTree<hr>"))
    ),
    mainPanel(
      # Show a simple table.
      shinyTree("tree", dragAndDrop = TRUE,types= #Types is in the same format that jstree expects
      "{
          '#': { 'max_children' : 2, 'max_depth' : 4, 'valid_children' : ['root'] },
          'root' : { 'valid_children' : ['file'] },
          'default' : { 'valid_children' : ['default','file'] },
          'file' : { 'icon' : 'fa fa-file', 'valid_children' : [] }
        }"
      )
    )
  ))
