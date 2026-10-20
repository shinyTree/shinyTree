library(shiny)
library(shinyTree)

## Controlled reproduction of the #36 feedback loop.
##
## Both callback options are enabled, which is the only configuration where the
## loop is possible: updateTree() triggers refresh.jstree and set_state.jstree,
## and with these options those events notify the server, which calls
## updateTree() again.
##
## The observer is capped so the loop terminates and can be measured instead of
## hanging the test. CAP is deliberately far above the handful of updates a
## correctly behaving app produces.
options(shinyTree.refresh = TRUE)
options(shinyTree.setState = TRUE)

CAP <- 50

tree <- list(
  root1 = "",
  root2 = structure(list(leaf1 = "", leaf2 = ""), stopened = TRUE)
)

ui <- fluidPage(
  shinyTree("tree"),
  actionButton("server_update", "Server-side updateTree"),
  verbatimTextOutput("updates"),
  verbatimTextOutput("capped")
)

server <- function(input, output, session) {
  output$tree <- renderTree(tree)

  n <- reactiveVal(0)

  # The naive pattern from the issue: react to the tree, then update the tree.
  observeEvent(input$tree, {
    n(n() + 1)
    if (n() < CAP) {
      updateTree(session, "tree", tree)
    }
  })

  output$updates <- renderPrint(cat(n()))
  output$capped  <- renderPrint(cat(if (n() >= CAP) "CAPPED" else "bounded"))
}

shinyApp(ui, server)
