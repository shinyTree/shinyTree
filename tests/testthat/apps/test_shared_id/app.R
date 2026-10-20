library(shiny)
library(shinyTree)

## A tree is an input and an output under the same id by design, so Shiny's
## shared-id check must not report it.
ui <- fluidPage(
  shinyTree("tree", checkbox = TRUE)
)

server <- function(input, output, session) {
  output$tree <- renderTree(list(root1 = "", root2 = list(leaf = "")))
}

shinyApp(ui, server)
