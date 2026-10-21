library(shiny)
library(shinyTree)

## Expand all / Collapse all buttons driving expandTree()/collapseTree() (#42).
## root1 starts open; everything else is closed.
ui <- fluidPage(
  actionButton("expand", "Expand all"),
  actionButton("collapse", "Collapse all"),
  shinyTree("tree", checkbox = TRUE)
)

server <- function(input, output, session) {
  output$tree <- renderTree({
    list(
      root1 = structure(list(
        a = list(a1 = "", a2 = ""),
        b = list(b1 = structure("", stselected = TRUE))
      ), stopened = TRUE),
      root2 = list(c = list(c1 = list(c11 = "")))
    )
  })
  observeEvent(input$expand, expandTree(session, "tree"))
  observeEvent(input$collapse, collapseTree(session, "tree"))
}

shinyApp(ui, server)
