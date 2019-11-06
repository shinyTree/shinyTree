library(shiny)
library(shinyTree)

## OPTIONS #####################
options(shinyTree.setState = FALSE)
options(shinyTree.refresh  = FALSE)

## TREE DATA ###################
tree <- list(a = list(a1 = 1, a2 = 2), b = "b") 
tree <- lapply(tree, function(x) structure(x, stopened = TRUE))

## UI ######################
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      checkboxInput("refresh", "options(shinyTree.refresh)"),
      checkboxInput("set_state", "options(shinyTree.setState)"),
      actionButton('reset', 'Reset nodes'),
      hr(),
      helpText(HTML(
        "This example shall illustrate the two options:<br>", 
        "<code>shinyTree.setState</code> and <code>shinyTree.refresh</code>.<br><br>",
        "By default, both options are <b>FALSE</b> and the <em>Selected Nodes</em> output does not",
        "change after clicking on <kbd>Reset nodes</kbd>. The selection is retained.<br><br>",
        "If either option is set to <b>TRUE</b>, the <em>Selected Nodes</em> output",
        "is reset to <code>list()</code>."
        ))
    ),
    mainPanel(
      uiOutput("tree_placeholder"),
      hr(),
      h4("Selected nodes:"),
      verbatimTextOutput("idSelected")
    )
  )
)

## Server ######################
server <- function(input, output, session) {


  output$idSelected <- renderPrint({
    str(get_selected(req(input$tree), format = "classid"))
  })
  
  # An observer is used to trigger a tree update when reset is clicked.
  observeEvent(input$reset, {
    updateTree(session, "tree", data = tree)
  })
  
  observe({
    options(shinyTree.setState = input$set_state)
    options(shinyTree.refresh = input$refresh)
    output$tree_placeholder <- renderUI(shinyTree("tree"))
    output$tree <- renderTree({
      attr(tree[[1]][[2]], "stselected") <- TRUE
      tree
    })
  })
}

shinyApp(ui, server)