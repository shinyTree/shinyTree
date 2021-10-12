library(shiny)
library(shinyTree)

shinyServer(function(input, output, session) {
  
  output$tree <- renderTree({
    list(
      root1 = "123",
      root2 = list(
        SubListA = list(leaf1 = "", leaf2 = "", leaf3=""),
        SubListB = structure(list(leafA = "", leafB = ""), stchecked=TRUE)
      )
    )
  })
})
