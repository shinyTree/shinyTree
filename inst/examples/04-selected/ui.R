library(shiny)
library(shinyTree)

#' Define UI for application that demonstrates a simple Tree editor
#' @author Jeff Allen \email{jeff@@trestletech.com}
shinyUI(
  pageWithSidebar(
    # Application title
    headerPanel("shinyTree with 'selected' input"),
    
    sidebarPanel(
      helpText(HTML("An example of using shinyTree's <code>selected</code> parameter to monitor which cells are currently selected.
                  <hr>Created using <a href = \"http://github.com/trestletech/shinyTree\">shinyTree</a>."))
    ),
    mainPanel(
      "Currently Selected:",
      verbatimTextOutput("selTxt"),
      hr(),
      shinyTree("tree", selected="treeSel")
    )
  ))