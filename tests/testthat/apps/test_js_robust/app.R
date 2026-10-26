## Browser tests for shinyTree.js edge cases, driven by tests/playwright/run.js.
library(shiny)
library(shinyTree)

tree_a <- list(a1 = "", a2 = list(a21 = ""))
tree_b <- list(b1 = "", b2 = "")

ui <- fluidPage(
  checkboxInput("show", "show tree", TRUE),
  actionButton("swap", "swap"),
  actionButton("update_hidden", "update hidden"),
  actionButton("expand_hidden", "expand hidden"),
  # Re-rendered after a req() failure: input$tree must keep tracking clicks.
  shinyTree("tree", contextmenu = TRUE),
  verbatimTextOutput("report"),
  # Never shown, so its output is never bound or rendered.
  conditionalPanel("false", shinyTree("hidden")),
  verbatimTextOutput("hidden_report")
)

server <- function(input, output, session) {
  which <- reactiveVal("a")
  observeEvent(input$swap, which(if (which() == "a") "b" else "a"))
  output$tree <- renderTree({
    req(input$show)
    if (which() == "a") tree_a else tree_b
  })
  output$report <- renderText({
    t <- input$tree
    sel <- unlist(get_selected(t))
    paste0("names: ", paste(names(t), collapse = ","),
           " | selected: ", paste(sel, collapse = ","))
  })
  observeEvent(input$update_hidden, updateTree(session, "hidden", list(h = "")))
  observeEvent(input$expand_hidden, expandTree(session, "hidden"))
  output$hidden_report <- renderText(paste("updates sent:", input$update_hidden + input$expand_hidden))
}

shinyApp(ui, server)
