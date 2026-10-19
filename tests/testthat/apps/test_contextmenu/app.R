library(shiny)
library(shinyTree)

ui <- fluidPage(
  shinyTree("tree", contextmenu = TRUE),
  verbatimTextOutput("tree_str")
)

server <- function(input, output, session) {
  output$tree <- renderTree({
    list(
      root1 = "",
      root2 = ""
    )
  })
  output$tree_str <- renderPrint({
    tree <- req(input$tree)
    str(tree)
  })
}

shinyApp(ui, server)
