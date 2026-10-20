library(shiny)
library(shinyTree)

## tie_selection = FALSE with the default three_state = TRUE, where checking a
## parent cascades to its children and unchecking a child cascades to its parent.
ui <- fluidPage(
  shinyTree("tree", checkbox = TRUE, tie_selection = FALSE),
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
    cat(vapply(get_checked(tree), `[`, "", 1), sep = "\n")
  })
}

shinyApp(ui, server)
