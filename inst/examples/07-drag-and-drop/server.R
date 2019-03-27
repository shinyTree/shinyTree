library(shiny)
library(shinyTree)

#' Define server logic required to generate a simple tree
#' @author Jeff Allen \email{jeff@@trestletech.com}
shinyServer(function(input, output, session) {
  output$tree <- renderTree({
    list(
      root3 = "234",
      root1 = list(
        SubListA = list(leaf1 = "", leaf2 = "")
      ),
      root2 = list(
        SubListA = list(leaf1 = "", leaf2 = "")
      )
    )
  })
  
  output$str <- renderPrint({
    # shinyTrees will also be available as inputs so you can
    # monitor changes that occur to the tree as the user interacts
    # with it.
    str(input$tree)
  })
})