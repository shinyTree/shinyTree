library(shiny)
library(shinyTree)

## Alongside a tree, a genuine mistake: an input and an output sharing an id.
## Shiny's warning about it must still be shown.
ui <- fluidPage(
  shinyTree("tree"),
  textInput("dup", "dup"),
  textOutput("dup")
)

server <- function(input, output, session) {
  output$tree <- renderTree(list(root1 = "", root2 = list(leaf = "")))
  output$dup <- renderText("dup")
}

shinyApp(ui, server)
