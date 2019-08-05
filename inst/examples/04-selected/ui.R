library(shiny)
library(shinyTree)

#' Define UI for application that demonstrates a simple Tree editor
#' @author Jeff Allen \email{jeff@@trestletech.com}
shinyUI(
  pageWithSidebar(
    # Application title
    headerPanel("shinyTree with 'selected' input"),
    
    sidebarPanel(
      helpText(HTML("An example of using shinyTree's <code>get_selected</code> function to extract the cells which are currently selected.
                  <hr>Created using <a href = \"http://github.com/trestletech/shinyTree\">shinyTree</a>."))
    ),
    mainPanel(
      "Currently Selected:",
      shinyTree("tree123"),
      hr(),
      "Currently Selected:",
      verbatimTextOutput("sel_names"),
      verbatimTextOutput("sel_slices"),
      verbatimTextOutput("sel_classid")
    )
  ))