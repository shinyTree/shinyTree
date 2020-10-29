library(shiny)
library(shinyTree)

#' Example of creating a tree from a datatable and converting the tree back to datatable
shinyServer(function(input, output, session) {
  hierarchy <- c("Sex", "Class", "Survived")
  output$tree <- renderTree({
    df <- data.frame(Titanic)
    tree <- dfToTree(df, hierarchy)
    #shiny tree must have a selected element to return the tree in input
    attr(tree[[1]],"stselected") <- TRUE 
    tree
  })
  
  output$df <- renderTable({
    req(input$tree)
    treeToDf(input$tree,hierarchy)
  })
})