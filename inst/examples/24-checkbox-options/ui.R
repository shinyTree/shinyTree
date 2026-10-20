library(shiny)
library(shinyTree)

#' Define UI for application that demonstrates a simple Tree editor
#' @author Jeff Allen \email{jeff@@trestletech.com}
shinyUI(
  pageWithSidebar(
    # Application title
    headerPanel("shinyTree with checkbox options"),
    
    sidebarPanel(
      helpText(HTML("An example of a shinyTree with the <code>checkbox</code> parameter enabled but with options <code>tie_selection, whole_node</code>, and  <code>three_state</code> set to false  such that checked nodes are not tied to selected nodes and can be manipulated by themselves.
                  Click a label to select a node and tick a box to check one, and the two lists below change independently.
                  See the <code>04-selected</code> example to try every combination of these options.
                  <hr>Created using <a href = \"http://github.com/trestletech/shinyTree\">shinyTree</a>."))
    ),
    mainPanel(
      # Show a simple table.
      shinyTree("tree", checkbox = TRUE, three_state=FALSE, whole_node=FALSE, tie_selection=FALSE),
      hr(),
      fluidRow(
        column(6, "get_selected():", verbatimTextOutput("selected")),
        column(6, "get_checked():", verbatimTextOutput("checked"))
      )
  ))
)
