library(shiny)
library(shinyTree)
library(data.tree)

ui <-   pageWithSidebar(
  headerPanel("Drag-and-Drop shinyTree with data.tree"),
  
  sidebarPanel(      
    "List of Lists",
    shinyTree("tree", dragAndDrop=TRUE),
    "Data Tree",
    shinyTree("tree2", dragAndDrop=TRUE),
    HTML("<hr />"),
    helpText(p("Drag some of the nodes in the tree around to see the structure on the right update."),
             HTML("<hr />Created using <a href = \"http://github.com/trestletech/shinyTree\">shinyTree</a>."),
             HTML("<hr />Source updated at <a href = \"http://github.com/ColumbusCollaboratory/shinyTree\">ColumbusCollaborator/shinyTree</a>."))
  ),
  mainPanel(
    verbatimTextOutput("listtreeOutput"),
    verbatimTextOutput("datatreeOutput")  
  )
)

#' Define server logic required to generate a simple tree
#' @author Jeff Allen \email{jeff@@trestletech.com}
server <- function(input, output, session) {
  output$tree <- renderTree({
    list(
      root1 = "123",
      root2 = list(
        SubListA = list(leaf1 = "", leaf2 = "")
      )
    )
    
  })
  
  output$tree2 <- renderTree({
    top <- Node$new("TOP")
    root1 <- top$AddChild("root1")
    root2 <- top$AddChild("root2")
    SubListA <- root2$AddChild("SubListA")
    leaf1 <- SubListA$AddChild("leaf1")
    leaf2 <- SubListA$AddChild("leaf2")
    
    top
    
  })  
  output$listtreeOutput <- renderPrint({
    # shinyTrees will also be available as inputs so you can
    # monitor changes that occur to the tree as the user interacts
    # with it.
    str(input$tree)
  })
  output$datatreeOutput <- renderPrint({
    # shinyTrees will also be available as inputs so you can
    # monitor changes that occur to the tree as the user interacts
    # with it.
    str(input$tree2)
  })  
}

shinyApp(ui = ui, server = server)

