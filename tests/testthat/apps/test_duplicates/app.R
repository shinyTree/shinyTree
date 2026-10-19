library(shiny)
library(shinyTree)

## Mirrors the tree from issue #104: SubListB holds four siblings all named
## "leafA", each carrying its own stinfo, plus a distinctly named leafB.
ui <- fluidPage(
  shinyTree("tree"),
  verbatimTextOutput("sel_info")
)

server <- function(input, output, session) {
  output$tree <- renderTree({
    list(
      root1 = structure("123", stinfo = "A"),
      SubListB = structure(
        list(
          leafA = structure("", stinfo = "Info LeafA1"),
          leafA = structure("", stinfo = "Info LeafA2"),
          leafA = structure("", stinfo = "Info LeafA3"),
          leafB = structure("", stinfo = "Info LeafB"),
          leafA = structure("", stinfo = "Info LeafA4")
        ),
        stopened = TRUE,
        stinfo = "Info SubListB"
      )
    )
  })

  output$sel_info <- renderPrint({
    tree <- req(input$tree)
    cat(paste(
      vapply(get_selected(tree), function(node) {
        paste0(node, ":", attr(node, "stinfo"))
      }, character(1)),
      collapse = "\n"
    ))
  })
}

shinyApp(ui, server)
