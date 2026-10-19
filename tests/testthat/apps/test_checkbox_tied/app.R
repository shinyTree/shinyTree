library(shiny)
library(shinyTree)

## checkbox = TRUE with the default tie_selection = TRUE, where jsTree treats
## checked and selected as the same thing.
ui <- fluidPage(
  shinyTree("tree", checkbox = TRUE),
  verbatimTextOutput("checked_names")
)

server <- function(input, output, session) {
  output$tree <- renderTree({
    list(
      root1 = "",
      root2 = structure(
        list(
          leaf1 = "",
          leaf2 = ""
        ),
        stopened = TRUE
      )
    )
  })
  output$checked_names <- renderPrint({
    tree <- req(input$tree)
    print(get_checked(tree))
  })
}

shinyApp(ui, server)
