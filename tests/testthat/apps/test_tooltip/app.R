library(shiny)
library(shinyTree)

## Nodes with sttooltip show it on hover (#66). The text includes markup and
## quotes to check it is shown as text, not interpreted.
ui <- fluidPage(
  shinyTree("tree", checkbox = TRUE),
  verbatimTextOutput("info")
)

server <- function(input, output, session) {
  output$tree <- renderTree({
    list(
      root1 = structure("", sttooltip = "The first root"),
      root2 = structure(list(
        leaf1 = structure("", sttooltip = "Say \"hi\" <b>not bold</b>", stinfo = "x"),
        leaf2 = ""
      ), stopened = TRUE)
    )
  })
  output$info <- renderPrint({
    tree <- req(input$tree)
    cat(vapply(get_checked(tree), function(n) paste(n, attr(n, "sttooltip") %||% "-"), ""), sep = "\n")
  })
}

shinyApp(ui, server)
