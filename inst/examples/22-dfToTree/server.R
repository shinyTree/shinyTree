library(shiny)
library(shinyTree)

#' Example of creating a tree from a datatable and converting the tree back to datatable
shinyServer(function(input, output, session) {
  hierarchy <- c("Sex", "Class", "Survived")
  output$tree <- renderTree({
    df <- data.frame(Titanic)
    dfToTree(df, hierarchy)
  })
  
  output$df <- renderTable({
    req(input$tree)
    treeToDf(input$tree,hierarchy)
  })
})