library(shiny)
library(shinyTree)

## A tree wrapped in bindCache inside a modalDialog. A cache hit must send the
## cached tree, and reopening the modal must resend whichever tree was last sent,
## whether it came from a render or from the cache (#109, #79).
##
## The render counter follows test_modal_rerender: a plain variable updated
## with `<<-` and reported through an isolated reactiveVal.
ui <- fluidPage(
  radioButtons("key", "key", c("a", "b"), inline = TRUE),
  actionButton("show", "Show modal"),
  verbatimTextOutput("render_count")
)

server <- function(input, output, session) {
  renders <- 0
  reported <- reactiveVal(0)

  output$tree <- bindCache(renderTree({
    renders <<- renders + 1
    isolate(reported(renders))
    structure(list(x = "", y = ""), names = paste0(input$key, 1:2))
  }), input$key)

  output$render_count <- renderPrint(cat(reported()))

  observeEvent(input$show, {
    showModal(modalDialog(shinyTree("tree")))
  })
}

shinyApp(ui, server)
