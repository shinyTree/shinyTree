library(shiny)
library(shinyTree)

## Regression app for #34, the behaviour this change is most likely to break.
##
## #34: the user makes a change, the server rejects it and restores the tree
## with updateTree(), then the user makes the *same* change again. Because the
## payload is identical, Shiny would normally discard it as a duplicate and the
## server would never hear about the second attempt -- leaving the rejected
## change on screen. The callbackCounter in shinyTree.js exists to keep each
## message unique so that cannot happen.
##
## Suppressing the updateTree() echo must not re-break this: the user's repeat
## attempt has to arrive. Here "root2" is forbidden, and every rejection is
## counted so a test can prove the second attempt was received.
tree <- list(
  root1 = "",
  root2 = "",
  root3 = ""
)

ui <- fluidPage(
  shinyTree("tree"),
  verbatimTextOutput("rejections"),
  verbatimTextOutput("selection")
)

server <- function(input, output, session) {
  output$tree <- renderTree(tree)

  rejections <- reactiveVal(0)

  observeEvent(input$tree, {
    sel <- get_selected(input$tree)
    names_sel <- vapply(sel, function(n) n[1], character(1))
    if ("root2" %in% names_sel) {
      # Reject: restore the tree with nothing selected.
      rejections(rejections() + 1)
      updateTree(session, "tree", tree)
    }
  })

  output$rejections <- renderPrint(cat(rejections()))
  output$selection <- renderPrint({
    sel <- get_selected(req(input$tree))
    cat(paste(vapply(sel, function(n) n[1], character(1)), collapse = ","))
  })
}

shinyApp(ui, server)
