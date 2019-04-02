library(shiny)
library(shinyTree)

js <- HTML("
$(document).on('shiny:connected', function(event) {
  $('#tree').on('hover_node.jstree', function (e, data) {
    $('#'+data.node.id).prop('title', 'The node id is: ' + data.node.id + 
                                      ' \\nThe node text is: ' + data.node.text);
  })
});")

ui <- {fluidPage(
  tags$head(tags$script(js)),
  shinyTree("tree")
)}

server <- function(input, output, session) {
  output$tree <- renderTree({
    list(
      root1 = structure("123"),
      root2 = list(
        SubListA = list(leaf1 = "", leaf2 = "", leaf3=""),
        SubListB = list(leafA = "", leafB = "")
      ),
      root3 = structure("123"),
      root4 = structure("123"),
      root5 = structure("123"),
      root6 = structure("123")
    )
  })
}

shinyApp(ui, server)