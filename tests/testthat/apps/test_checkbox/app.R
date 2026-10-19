library(shiny)
library(shinyTree)

ui <- fluidPage(
  shinyTree("tree", checkbox = TRUE),
  verbatimTextOutput("selected_names")
)

server <- function(input, output, session) {
  output$tree <- renderTree({
    list(
      root1 = "",
      root2 = list(
        leaf1 = "",
        leaf2 = ""
      )
    )
  })
  output$selected_names <- renderPrint({
    tree <- req(input$tree)
    print(get_selected(tree))
  })
}

shinyApp(ui, server)
