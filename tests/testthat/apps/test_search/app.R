library(shiny)
library(shinyTree)

ui <- fluidPage(
  shinyTree("tree", search = TRUE, searchtime = 50),
  verbatimTextOutput("selected_names")
)

server <- function(input, output, session) {
  output$tree <- renderTree({
    list(
      apple = "",
      banana = "",
      cherry = ""
    )
  })
  output$selected_names <- renderPrint({
    tree <- req(input$tree)
    print(get_selected(tree))
  })
}

shinyApp(ui, server)
