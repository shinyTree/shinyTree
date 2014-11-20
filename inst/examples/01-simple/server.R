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
      )
    )
  })
})