library(shiny)
library(shinyTree)

#' Define UI for application that demonstrates a simple Tree editor
#' @author Jeff Allen \email{jeff@@trestletech.com}
shinyUI(
  pageWithSidebar(
    headerPanel("Drag-and-Drop shinyTree"),
    
    sidebarPanel(      
      shinyTree("tree", dragAndDrop=TRUE),
      HTML("<hr />"),
      helpText(p("Drag some of the nodes in the tree around to see the structure on the right update."),
               HTML("<hr />Created using <a href = \"http://github.com/trestletech/shinyTree\">shinyTree</a>."))
    ),
    mainPanel(
      verbatimTextOutput("str")  
    )
  ))