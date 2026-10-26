library(shiny)
library(shinyTree)

## The #36 loop with an observer that sends back a tree that differs from
## input$tree each time: it marks the nodes the user selected as opened. A
## server-sent tree is reported to the server once if it differs from what
## input$tree holds, so this is the case that could loop: each report would
## trigger a new, different tree. It must settle instead, because the tree it
## sends back is the same each time once the nodes are marked.
##
## Capped like test_update_loop so a runaway is measured rather than hanging.
CAP <- 50

ui <- fluidPage(
  shinyTree("tree"),
  verbatimTextOutput("updates"),
  verbatimTextOutput("capped")
)

server <- function(input, output, session) {
  output$tree <- renderTree(list(a = list(a1 = ""), b = list(b1 = "")))

  n <- reactiveVal(0)
  observeEvent(input$tree, {
    n(n() + 1)
    if (n() >= CAP) return()
    tree <- input$tree
    for (name in names(tree)) {
      if (isTRUE(attr(tree[[name]], "stselected"))) {
        attr(tree[[name]], "stopened") <- TRUE
      }
    }
    updateTree(session, "tree", tree)
  })

  output$updates <- renderPrint(cat(n()))
  output$capped  <- renderPrint(cat(if (n() >= CAP) "CAPPED" else "bounded"))
}

shinyApp(ui, server)
