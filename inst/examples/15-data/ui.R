library(shiny)
library(shinyTree)


#' Example of storing and retrieving data in the tree

shinyUI(
  pageWithSidebar(
    # Application title
    headerPanel("shinyTree data with 'selected' input"),
    
    
    sidebarPanel(
      helpText(HTML("An example of storing and retrieving data in the tree. Select root1 or leaf1 to see stored data."))
    ),
    mainPanel(
      "Currently Selected:",
      verbatimTextOutput("selTxt"),
      hr(),
      shinyTree("tree")
    )
  ))