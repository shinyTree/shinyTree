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

  # With tie_selection = FALSE these are independent: compare the two after
  # clicking a label and after ticking a box.
  output$selected <- renderPrint({
    tree <- req(input$tree)
    str(get_selected(tree))
  })
  output$checked <- renderPrint({
    tree <- req(input$tree)
    str(get_checked(tree))
  })
})
