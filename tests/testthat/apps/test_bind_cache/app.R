library(shiny)
library(shinyTree)

## renderTree wrapped in bindCache, keyed on a radio button. The expression must
## run once per key, and a cache hit must still send the cached tree (#109).
ui <- fluidPage(
  radioButtons("key", "key", c("a", "b"), inline = TRUE),
  shinyTree("tree"),
  verbatimTextOutput("render_count")
)

server <- function(input, output, session) {
  renders <- 0
  reported <- reactiveVal(0)

  output$tree <- bindCache(renderTree({
    renders <<- renders + 1
    ## Report the new count without taking a reactive dependency on it.
    isolate(reported(renders))
    structure(list(leaf = ""), names = paste0("tree_", input$key))
  }), input$key)

  output$render_count <- renderPrint(cat(reported()))
}

shinyApp(ui, server)
