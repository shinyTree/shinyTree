library(shiny)
library(shinyTree)

## Companion to test_events_all for the four checkbox events. shinyTree()
## force-disables checkbox when contextmenu is enabled, so these cannot share
## one app. tie_selection = FALSE is required for check_node/uncheck_node/
## check_all/uncheck_all to fire at all (jsTree routes through selection when
## the two are tied).
options(shinyTree.refresh = TRUE)
options(shinyTree.setState = TRUE)

tree <- list(
  root1 = "",
  root2 = structure(list(leaf1 = "", leaf2 = ""), stopened = TRUE)
)

ui <- fluidPage(
  shinyTree("tree", checkbox = TRUE, tie_selection = FALSE),
  actionButton("server_update", "Server-side updateTree"),
  verbatimTextOutput("updates")
)

server <- function(input, output, session) {
  output$tree <- renderTree(tree)

  n <- reactiveVal(0)
  observeEvent(input$tree, { n(n() + 1) })

  observeEvent(input$server_update, {
    updateTree(session, "tree", tree)
  })

  output$updates <- renderPrint(cat(n()))
}

shinyApp(ui, server)
