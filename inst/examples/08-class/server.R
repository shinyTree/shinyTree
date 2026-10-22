library(shiny)
library(shinyTree)

#' Define server logic demonstrating custom CSS classes on nodes
#' @author Jeff Allen \email{jeff@@trestletech.com}
shinyServer(function(input, output, session) {
  output$tree <- renderTree({
    list(
      # A jsTree type styles every node of that type; a node has one type.
      root1 = structure("", sttype="red-node"),
      root2 = structure(list( SubListA = list(leaf1 = "", leaf2 = "")), sttype="blue-node"),
      # stcssclass adds any number of CSS classes, styled by the app's own CSS.
      tasks = structure(list(
        `Write report` = structure("", stcssclass = c("done", "important")),
        `Book travel`  = structure("", stcssclass = "done"),
        `Call client`  = structure("", stcssclass = "important"),
        `Tidy desk`    = ""
      ), stopened = TRUE)
    )
  })
})
