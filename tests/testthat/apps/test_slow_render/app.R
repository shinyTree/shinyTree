library(shiny)
library(shinyTree)

## A render expression that takes longer than the client's rebind check, as a
## database query might. The tree must be sent once, not twice: asking for a
## resend on first load made the server send it again and jsTree rebuild it.
##
## The render counter follows test_modal_rerender: a plain variable updated
## with `<<-` and reported through an isolated reactiveVal.
ui <- fluidPage(
  shinyTree("tree"),
  verbatimTextOutput("render_count")
)

server <- function(input, output, session) {
  renders <- 0
  reported <- reactiveVal(0)

  output$tree <- renderTree({
    Sys.sleep(0.5)
    renders <<- renders + 1
    isolate(reported(renders))
    list(root1 = "", root2 = list(leaf = ""))
  })

  output$render_count <- renderPrint(cat(reported()))
}

shinyApp(ui, server)
