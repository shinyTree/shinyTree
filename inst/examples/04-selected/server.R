library(shiny)
library(shinyTree)

#' Define server logic required to generate a simple tree
#' @author Jeff Allen \email{jeff@@trestletech.com}
shinyServer(function(input, output, session) {
  
  output$tree123 <- renderTree({
    list(
      root1 = structure("123"),
      root2 = list(
        SubListA = list(leaf1 = "", leaf2 = "", leaf3=""),
        SubListB = list(leafA = "", leafB = "")
      )
    )
  })

  output$sel_names <- renderPrint({
    tree <- input$tree123
    req(tree)
    get_selected(tree)
  })
  output$sel_slices <- renderPrint({
    tree <- input$tree123
    req(tree)
    get_selected(tree, format = "slices")
  })
  output$sel_classid <- renderPrint({
    tree <- input$tree123
    req(tree)
    get_selected(tree, format = "classid")
  })
})