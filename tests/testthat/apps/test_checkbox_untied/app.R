library(shiny)
library(shinyTree)

ui <- fluidPage(
  shinyTree("tree", checkbox = TRUE, tie_selection = FALSE),
  verbatimTextOutput("checked_names")
)

server <- function(input, output, session) {
  output$tree <- renderTree({
    list(
      root1 = "",
      root2 = ""
    )
  })
  output$checked_names <- renderPrint({
    tree <- req(input$tree)
    print(get_checked(tree))
  })
}

shinyApp(ui, server)
