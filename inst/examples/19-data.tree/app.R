library(data.tree)
library(shiny)
library(shinyTree)

data(acme)
acme$IT$li_attr <- list(class = "myl")
acme$IT$state <- list(opened = TRUE)
acme$Accounting$icon <- "file"
acme$IT$Outsource$state <- list(opened = TRUE)
options(shinyTree.defaultParser = "tree")

ui <- fluidPage(
  tags$head(
    tags$style(HTML(".myl {color: red}"))),
  shinyTree("tree", dragAndDrop = TRUE),
  fluidRow(
    column(width = 6,
           h3("Tree"),
           verbatimTextOutput("str_tree")),
    column(width = 6,
           h3("json"),
           verbatimTextOutput("str_json")
    )
  ))

server <- function(input, output, session) {
  get_json <- reactive({
    treeToJSON(acme, pretty = TRUE)
  })
  output$tree <- renderTree(get_json())
  output$str_json <- renderPrint(cat(get_json()))
  output$str_tree <- renderPrint(do.call(print, c(x = req(input$tree), 
                                                  as.list(input$tree$fieldsAll))))
}

shinyApp(ui, server)