library(shiny)
library(shinyTree)

#' Define UI for application that demonstrates a simple Tree editor
#' @author Jeff Allen \email{jeff@@trestletech.com}
shinyUI(
  pageWithSidebar(
    headerPanel("shinyTree structure"),
    
    sidebarPanel(      
      shinyTree("tree"),
      HTML("<hr />"),
      helpText(HTML("<p>An example of capturing the returned tree as an input in list 
                    form. Interact with the tree above to see the attributes of the 
                    structure on the right change. </p>
                    <p>This structure reveals every detail about the current state 
                    of the tree and can be parsed to derive whatever information you 
                    need about your tree.</p>
                  <hr />Created using <a href = \"http://github.com/trestletech/shinyTree\">shinyTree</a>."))
    ),
    mainPanel(
      verbatimTextOutput("str")  
    )
  ))