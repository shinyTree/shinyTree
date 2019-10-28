library(shiny)
library(shinyTree)

elem <- structure("", sttype = "elem")
my_tree <- list(root1 = structure(list(elem1 = elem, elem2 = elem, elem3 = elem), 
                                  sttype = "root"),
                root2 = structure(list(elem1 = elem, elem2 = elem, elem3 = elem), 
                                  sttype = "root"),
                root3 = structure(list(elem1 = elem, elem2 = elem, elem3 = elem),
                                  sttype = "root"))

get_part_tree <- function(scope) {
  part_tree <- my_tree[-sample(3, 1)]
  names(part_tree) <- paste(scope, names(part_tree), sep = "_")
  part_tree
}
tree_module_ui <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns("go"), "Go!"),
    h4("Tree with Types"),
    shinyTree(ns("tree_with_type"),
              types = "{'root': {'icon': 'fa fa-archive'}, 'elem':{'icon': 'fa fa-file'}}"),
    h4("Tree without Types"),
    shinyTree(ns("tree_without_type"))
  )
}
tree_module <- function(input, output, session) {
  output$tree_without_type <- renderTree({
    input$go
    get_part_tree("Module w/o Type")
  })
  
  output$tree_with_type <- renderTree({
    input$go
    get_part_tree("Module w/ Type")
  })
}

ui <- fluidPage(
  h3("Module 1"),
  tree_module_ui("tree_module1"),
  h3("Module 2"),
  tree_module_ui("tree_module2")
)

server <- function(input, output, session) {
  callModule(tree_module,
             "tree_module1")
  callModule(tree_module,
             "tree_module2")
}

shinyApp(ui, server)
