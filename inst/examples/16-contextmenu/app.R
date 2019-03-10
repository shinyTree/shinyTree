library(shiny)
library(shinyTree)

treedata =     list(
  root1 = structure("", sttype="red-node"),
  root2 = structure(list(
    SubListA = list(leaf1 = "", leaf2 = "")
  ), sttype="blue-node")
)

ui <- fluidPage(
  actionButton("update", "Update Tree"),
  shinyTree("tree2", contextmenu = TRUE, dragAndDrop = T, search = T, 
            #state=T,
            # themeIcons = TRUE, themeDots = TRUE, unique = T, checkbox = F,
            types = "{
               'red-node': {'a_attr' : { 'style' : 'color:red' }},
               'blue-node': {'a_attr' : { 'style' : 'color:blue' }}
            }"
  ),
  verbatimTextOutput("sel_names"),
  verbatimTextOutput("sel_slices"),
  verbatimTextOutput("sel_id")
)

server <- function(input, output, session) {
  output$tree2 <- renderTree({
    treedata
  })
  observeEvent(input$update, {
    updateTree(session, "tree2", treedata)
  })
  
  output$sel_names <- renderPrint({
    req(input$tree2)
    # browser()
    get_selected(input$tree2, "names")
  })
  output$sel_slices <- renderPrint({
    req(input$tree2)
    get_selected(input$tree2, "slices")
  })
  output$sel_id <- renderPrint({
    req(input$tree2)
    get_selected(input$tree2, "classid")
  })
}

shinyApp(ui, server)