library(shiny)
library(shinyTree)

## OPTIONS #####################
options(shinyTree.setState = FALSE)
# options(shinyTree.refresh = TRUE)

## TREE DATA ###################
tree = list(a=list(a1=1,a2=2) , b="b") 
tree = lapply(tree, function(x) structure(x, stopened = T))

## UI ######################
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
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
      shinyTree("tree", ),
      hr(),
      h4("Selected nodes:"),
      verbatimTextOutput("idSelected")
    )
  )
)

## Server ######################
server <- function(input, output, session) {
  output$tree = renderTree({
    attr(tree[[1]][[2]], "stselected") <- TRUE
    tree
  })
  
  output$idSelected <- renderPrint({
    str(get_selected(input$tree, format = "classid"))
  })
  
  # An observer is used to trigger a tree update when reset is clicked.
  observeEvent(input$reset, {
    updateTree(session,"tree", data = tree)
  })
}

shinyApp(ui, server)