library(shiny)
library(shinyTree)

#' Define server logic required to generate a simple tree
#' @author Jeff Allen \email{jeff@@trestletech.com}
shinyServer(function(input, output, session) {
  output$tree <- renderTree({
    list(
      root1 = structure("", stclass="red-node"),
      root2 = structure(list(
        SubListA = list(leaf1 = "", leaf2 = "")
      ), stclass="blue-node")
    )
  })
})