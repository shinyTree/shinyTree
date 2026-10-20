library(shiny)
library(shinyTree)

## Records every input$tree update so tests can assert which jsTree events do
## and do not reach the server. Both callback options are on, so refresh and
## set_state would notify the server if they were not suppressed.
##
## contextmenu is enabled here; shinyTree() force-disables checkbox when
## contextmenu is on, so the four checkbox events live in test_events_checkbox.
options(shinyTree.refresh = TRUE)
options(shinyTree.setState = TRUE)

tree <- list(
  root1 = "",
  root2 = structure(list(leaf1 = "", leaf2 = ""), stopened = TRUE)
)

ui <- fluidPage(
  shinyTree("tree", contextmenu = TRUE, dragAndDrop = TRUE),
  actionButton("server_update", "Server-side updateTree"),
  verbatimTextOutput("updates")
)

server <- function(input, output, session) {
  output$tree <- renderTree(tree)

  n <- reactiveVal(0)
  observeEvent(input$tree, { n(n() + 1) })

  # No updateTree() in the observer: this app measures event delivery, not the
  # loop, so a server update must be requested explicitly via the button.
  observeEvent(input$server_update, {
    updateTree(session, "tree", tree)
  })

  output$updates <- renderPrint(cat(n()))
}

shinyApp(ui, server)
