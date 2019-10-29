library(shiny)
library(shinyTree)

dt <- list(
  root1 = structure("123", stselected=T),
  root2 = list(
    SubListA = list(leaf1 = structure("leaf1"), leaf2 = "", leaf3=""),
    SubListB = list(leafA = "", leafB = "")
  )
)

ui <- fluidPage(
  shinyTree("tree"),
  actionButton("action", "Set selected on other node"),
  verbatimTextOutput("sel_names"),
  verbatimTextOutput("sel_classid"),
  verbatimTextOutput("sel_slices")
)

#' Example of using promises to render a tree asynchronously
server <- function(input, output, session) {
  output$tree <- renderTree({
    dt
  })
  observeEvent(input$action, {
    dt <- list(
      root1 = structure("123"),
      root2 = list(
        SubListA = list(leaf1 = structure("leaf1", stselected=T), leaf2 = "", leaf3=""),
        SubListB = list(leafA = "", leafB = "")
      )
    )
    attr(dt[[2]], "stopened") <- TRUE
    attr(dt[[2]][[1]], "stopened") <- TRUE
    updateTree(session, "tree", dt)
  })
  output$sel_names <- renderPrint({
    tree <- req(input$tree)
    sel <- get_selected(tree)
    print(sel)
  })
  output$sel_classid <- renderPrint({
    tree <- req(input$tree)
    sel <- get_selected(tree, "classid")
    print(sel)
  })
  output$sel_slices <- renderPrint({
    tree <- req(input$tree)
    sel <- get_selected(tree, "slices")
    print(sel)
  })
}

shinyApp(ui, server)

