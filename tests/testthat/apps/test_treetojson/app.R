library(shiny)
library(shinyTree)

## Mirrors the app from issue #103: the tree comes from treeToJSON() on a
## data.tree, whose state slot has no "checked" key.
n <- data.tree::as.Node(data.frame(
  pathString = c("./a/1/1", "./a/1/2/1", "./b/1", "./b/2"),
  Attr = c(1, 1, 2, 3),
  stringsAsFactors = FALSE
))

ui <- fluidPage(
  shinyTree("tree"),
  verbatimTextOutput("sel")
)

server <- function(input, output, session) {
  output$tree <- renderTree(treeToJSON(n, pretty = TRUE))
  output$sel <- renderPrint({
    tree <- req(input$tree)
    sel <- get_selected(tree)
    if (!length(sel)) {
      cat("(nothing selected)")
    } else {
      cat(paste(vapply(sel, function(node) node[1], character(1)), collapse = ","))
    }
  })
}

shinyApp(ui, server)
