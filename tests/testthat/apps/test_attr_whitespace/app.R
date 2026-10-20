library(shiny)
library(shinyTree)

## Node attributes are data, not display text: whatever the app stores must come
## back byte-for-byte. Here each node carries an stid whose whitespace is
## significant (fixed-width codes, padded keys) plus a non-string attribute.
ui <- fluidPage(
  shinyTree("tree"),
  verbatimTextOutput("attrs")
)

server <- function(input, output, session) {
  output$tree <- renderTree({
    list(
      alpha = structure("", stselected = TRUE, stid = "  padded-left"),
      beta  = structure("", stselected = TRUE, stid = "padded-right  "),
      gamma = structure("", stselected = TRUE, stid = "  both sides  "),
      delta = structure("", stselected = TRUE, stid = "inner  gap", stcount = 42L)
    )
  })

  output$attrs <- renderPrint({
    tree <- req(input$tree)
    for (name in names(tree)) {
      cat(sprintf("%s=[%s]\n", name, attr(tree[[name]], "stid")))
    }
    cat(sprintf("delta.stcount=%s\n", attr(tree$delta, "stcount")))
  })
}

shinyApp(ui, server)
