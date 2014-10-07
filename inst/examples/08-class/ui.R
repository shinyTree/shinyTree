library(shiny)
library(shinyTree)

#' Define UI for application that demonstrates a simple Tree editor
#' @author Jeff Allen \email{jeff@@trestletech.com}
shinyUI(
  pageWithSidebar(
    headerPanel("shinyTree with Custom CSS Class"),
    
    sidebarPanel(      
      includeCSS("customClass.css"),
      helpText(p(""),
               HTML("<hr />Created using <a href = \"http://github.com/trestletech/shinyTree\">shinyTree</a>."))
    ),
    mainPanel(
      shinyTree("tree")
    )
  ))