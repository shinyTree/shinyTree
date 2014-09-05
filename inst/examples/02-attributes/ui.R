library(shiny)
library(shinyTree)

#' Define UI for application that demonstrates a simple Tree editor
#' @author Jeff Allen \email{jeff@@trestletech.com}
shinyUI(
  pageWithSidebar(
    # Application title
    headerPanel("shinyTree with attributes!"),
    
    sidebarPanel(
      helpText(HTML("A shinyTree example with pre-defined attributes to dictate the behavior of specific nodes.
                  <hr>Created using <a href = \"http://github.com/trestletech/shinyTree\">shinyTree</a>."))
    ),
    mainPanel(
      # Show a simple table.
      shinyTree("tree")
    )
  ))