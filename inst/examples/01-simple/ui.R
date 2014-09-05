library(shiny)
library(shinyTree)

#' Define UI for application that demonstrates a simple Tree editor
#' @author Jeff Allen \email{jeff@@trestletech.com}
shinyUI(
  pageWithSidebar(
    # Application title
    headerPanel("Simple shinyTree!"),
    
    sidebarPanel(
      helpText(HTML("A simple Shiny Tree example.
                  <hr>Created using <a href = \"http://github.com/trestletech/shinyTree\">shinyTree</a>."))
    ),
    mainPanel(
      # Show a simple table.
      shinyTree("tree")
    )
  ))