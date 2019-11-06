library(shiny)
library(shinyjs)
library(shinyTree)

#' Define custom JS functions to implement jsTree core functionality
#' @author McClelland Legge \email{McClelland.Legge@@gmail.com}

shinyServer(function(input, output, session) {
  
  observeEvent(input$reset, {
    js$resetTree("tree")
  })
  
  output$tree <- renderTree({
    list(
      root1 = "123",
      root2 = list(
        SubListA = list(leaf1 = "", leaf2 = "", leaf3 = ""),
        SubListB = structure(list(leafA = "", leafB = ""), stselected = TRUE)
      )
    )
  })
  
})