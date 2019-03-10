library(shiny)
library(shinyTree)

css <- HTML("
.color { color: brown !important;}
.color1 { color: blue !important;}
.color2 { color: red !important;}
");

## Use 'stclass' for classes on a tags
# treedata =     list(
#   root1 = structure("", stclass="color", sttype="red-node"),
#   root2 = structure(stclass="color1", list(
#     SubListA = list(leaf1 = structure("", stclass="color2", sttype="red-node"), 
#                     leaf2 = "")
#   ), sttype="blue-node")
# )

## Use 'liclass' for classes on list tags
treedata =     list(
  root1 = structure("", stclass="color"),
  root2 = structure(liclass="color1", list(
    SubListA = list(leaf1 = "", 
                    leaf2 = "")
  )),
  root3 = structure(stclass="color2", list(
    SubListA = list(leaf1 = "", 
                    leaf2 = "")
  ))
)

ui <- fluidPage(
  tags$head(tags$style(css)),
  actionButton("update", "Update tree to start"),
  actionButton("browse", "Start browser"),
  shinyTree("tree2", contextmenu = TRUE, dragAndDrop = T, search = T, 
            state=TRUE,
            themeIcons = TRUE, themeDots = TRUE, unique = T, checkbox = T
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
    # updateTree(session, "tree2", input$tree2)
  })
  observeEvent(input$browse, {
    browser()
    input$tree2
  })
  
  output$sel_names <- renderPrint({
    req(input$tree2)
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