library(data.tree)
library(shiny)
library(shinyTree)
library(shinyjs)

## load acme and alter some nodes
data(acme)
acme$IT$li_attr         <- list(class = "myl")
acme$Accounting$icon    <- "file"


ui <- fluidPage(
  tags$head(
    tags$style(HTML(".myl {color: red}"))
  ),
  useShinyjs(),
  sidebarLayout(
    sidebarPanel(
      h2("Input"),
      verbatimTextOutput("input_tree"),
      helpText("This shinyTree control receives this ", code("data.tree"), " as input.",
               "In order to use that, ", code("treeToJSON(.)"), " is called with the tree.",
               "The resulting JSON can be found below."),
      verbatimTextOutput("json")
    ),
    mainPanel(
      h2("Output"),
      fluidRow(
        column(width = 4,
               h3("ShinyTree"),
               shinyTree("tree", dragAndDrop = TRUE)
        ),
        column(width = 7,
               h3(code("input$tree")),
               helpText("Change radio buttons once or change the tree to",
                        "force rendering of", code("input$tree")),
               radioButtons("parser",
                            "Parser:",
                            c("tree", "list")),
               verbatimTextOutput("output_tree"),
               helpText("As you can see, only attributes", code("cost"), "and",
                        code("p"), "are in slot", code("data"), ".",
                        code("li_attr"), "and", code("icon"), "were on the top level",
                        "of the node and are thus not returned by shinyTree.",
                        "State is always generated anew and hence also part of",
                        code("input$tree"), "despite sitting also on the top level.")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  print_tree <- function(tree) {
    if (is(tree, "Node")) {
      do.call(print, c(x = tree, as.list(tree$fieldsAll)))
    } else {
      str(tree)
    }
  }
  
  observe({
    options(shinyTree.defaultParser = input$parser)
    ## trigger "ready.jstree" by hand to force input$tree to update
    runjs("$('.jstree').trigger('ready.jstree')")
  })
  
  get_json <- reactive({
    treeToJSON(acme, pretty = TRUE)
  })
  
  output$input_tree  <- renderPrint(print_tree(acme))
  output$output_tree <- renderPrint(print_tree(req(input$tree)))
  output$json        <- renderPrint(cat(get_json()))   
  output$tree        <- renderTree(get_json())
}

shinyApp(ui, server)