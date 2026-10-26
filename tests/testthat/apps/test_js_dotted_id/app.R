## A tree whose id needs escaping in a CSS selector, next to an ordinary tree.
## Driven by tests/playwright/run.js.
library(shiny)
library(shinyTree)

ui <- fluidPage(
  shinyTree("my.tree", checkbox = TRUE, search = TRUE,
            types = "{ 'leaf': { 'icon': 'fa fa-leaf' } }"),
  verbatimTextOutput("dotted"),
  shinyTree("plain"),
  verbatimTextOutput("plain_report")
)

server <- function(input, output, session) {
  output[["my.tree"]] <- renderTree(list(x = structure("", sttype = "leaf"), y = ""))
  output$dotted <- renderText({
    t <- input[["my.tree"]]
    paste0("names: ", paste(names(t), collapse = ","),
           " | checked: ", paste(unlist(get_checked(t)), collapse = ","))
  })
  output$plain <- renderTree(list(p = ""))
  output$plain_report <- renderText(paste("names:", paste(names(input$plain), collapse = ",")))
}

shinyApp(ui, server)
