library(shiny)
library(shinyTree)

## Adversarial companion to test_modal_rerender: a tree that is *legitimately*
## empty. The client reports empty trees so the server can resend them (#79),
## and resending an empty tree leaves it empty -- so this must settle rather
## than resend forever.
##
## The counter uses `<<-` plus an isolated reactiveVal for the same reason as
## test_modal_rerender: incrementing a reactiveVal inside renderTree would make
## the render depend on its own output.
ui <- fluidPage(
  shinyTree("tree"),
  verbatimTextOutput("render_count")
)

server <- function(input, output, session) {
  renders <- 0
  reported <- reactiveVal(0)

  output$tree <- renderTree({
    renders <<- renders + 1
    isolate(reported(renders))
    list()
  })

  output$render_count <- renderPrint(cat(reported()))
}

shinyApp(ui, server)
