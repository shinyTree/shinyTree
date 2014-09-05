library(shiny)
library(shinyTree)

#' Define UI for application that demonstrates a simple Tree editor
#' @author Jeff Allen \email{jeff@@trestletech.com}
shinyUI(
  pageWithSidebar(
    # Application title
    headerPanel("shinyTree with checkbox controls"),
    
    sidebarPanel(
      helpText(HTML("An example of a shinyTree with the <code>checkbox</code> parameter enabled to allow users to more easily make multiple selections in the tree.
                  <hr>Created using <a href = \"http://github.com/trestletech/shinyTree\">shinyTree</a>."))
    ),
    mainPanel(
      # Show a simple table.
      shinyTree("tree", checkbox = TRUE)
  ))
)