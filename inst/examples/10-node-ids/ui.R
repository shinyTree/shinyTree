library(shiny)
library(shinyTree)

#' Define UI for application that demonstrates a simple Tree editor
#' @author Mike Schaffer \email{mschaff@gmail.com}
shinyUI(
  pageWithSidebar(
    # Application title
    headerPanel("shinyTree with node selection using node stids and stclass labels"),
    
    sidebarPanel(
      shinyTree("tree", checkbox=TRUE, theme="proton")
    ),
    mainPanel(
      "Currently Selected:",
      verbatimTextOutput("selTxt"),
      hr(),
      "Full Tree:",
      verbatimTextOutput("treeTxt")
    )
  ))
