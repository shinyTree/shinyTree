library(shiny)
library(shinyTree)

#' Define server logic required to generate a simple tree
#' @author Jeff Allen \email{jeff@@trestletech.com}
shinyServer(function(input, output, session) {
  output$tree <- renderTree({
    list(
      root1 = "",
      root2 = list(
        SubListA = list(leaf1 = "", leaf2 = "", leaf3=""),
        SubListB = list(leafA = "", leafB = "")
      ),
      root3 = list(
        SubListA = list(leaf1 = "", leaf2 = "", leaf3=""),
        SubListB = list(leafA = "", leafB = "")
      )
    )
  })
  
  output$sel_names <- renderPrint({
    out <- get_selected(input$tree, "names")
    req(out)
    out
  })
  output$sel_slices <- renderPrint({
    out <- get_selected(input$tree, "slices")
    req(out)
    out
  })
  output$sel_classid <- renderPrint({
    out <- get_selected(input$tree, "classid")
    req(out)
    out
  })
})