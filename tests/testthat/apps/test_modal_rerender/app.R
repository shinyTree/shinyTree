library(shiny)
library(shinyTree)

## Reproduction from issue #79: a tree inside a modalDialog. The tree renders on
## the first open, but a tree's contents are pushed to the client rather than
## returned as an output value, so Shiny has nothing to replay when the element
## is removed and added back -- and it will not re-run the render function,
## because the output is still considered valid.
##
## The render counter is written to a plain variable with `<<-` and exposed
## through a separate reactiveVal. Incrementing a reactiveVal *inside*
## renderTree would make the render depend on its own output and break the tree
## on the very first open.
tree <- list(a = list(a1 = 1, a2 = 2), b = "b")
tree <- lapply(tree, function(x) structure(x, stopened = TRUE))

ui <- fluidPage(
  actionButton("show", "Show modal"),
  verbatimTextOutput("render_count")
)

server <- function(input, output, session) {
  renders <- 0
  reported <- reactiveVal(0)

  output$tree <- renderTree({
    renders <<- renders + 1
    ## Report the new count without taking a reactive dependency on it.
    isolate(reported(renders))
    tree
  })

  output$render_count <- renderPrint(cat(reported()))

  observeEvent(input$show, {
    showModal(modalDialog(shinyTree("tree", search = TRUE)))
  })
}

shinyApp(ui, server)
