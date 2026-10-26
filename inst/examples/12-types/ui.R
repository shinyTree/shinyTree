library(shiny)
library(shinyTree)

#' Demonstrates types
#' @author Michael Bell \email{bellma@@lilly.com}
shinyUI(
  pageWithSidebar(
    # Application title
    headerPanel("shinyTree with types!"),
    
    sidebarPanel(
      helpText(HTML("A shinyTree example using types to dictate the behavior of specific nodes.
                  <hr>Created using shinyTree<hr>The Update button sends the same tree again with updateTree(), to show it gives the same result as renderTree().")),
      actionButton(inputId = "updateTree", "Update")
    ),
    mainPanel(
      # Show a simple table.
      shinyTree("tree", dragAndDrop = TRUE,types= #Types is in the same format that jstree expects
      "{
          '#': { 'max_children' : 2, 'max_depth' : 4, 'valid_children' : ['root'] },
          'root' : { 'icon' : 'fa fa-signal', 'valid_children' : ['file'] },
          'default' : { 'valid_children' : ['default','file'] },
          'file' : { 'icon' : 'glyphicon glyphicon-file', 'valid_children' : [] }
        }"
      )
    )
  ))
