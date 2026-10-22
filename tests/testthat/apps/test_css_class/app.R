library(shiny)
library(shinyTree)

## Nodes given CSS classes with stcssclass, styled by the app's own CSS (#58).
ui <- fluidPage(
  tags$style(HTML("
    .shiny-tree li.archived > a { color: rgb(128, 128, 128); }
    .shiny-tree li.urgent > a   { font-weight: 700; }
  ")),
  shinyTree("tree", checkbox = TRUE),
  verbatimTextOutput("report")
)

server <- function(input, output, session) {
  output$tree <- renderTree({
    list(
      projects = structure(list(
        alpha = structure("", stcssclass = c("archived", "urgent")),
        beta  = structure("", stcssclass = "urgent"),
        gamma = ""
      ), stopened = TRUE, stcssclass = "group"),
      delta = ""
    )
  })
  output$report <- renderPrint({
    tree <- req(input$tree)
    sel <- get_selected(tree)
    cat("selected:", vapply(sel, `[`, "", 1), "\n")
  })
}

shinyApp(ui, server)
