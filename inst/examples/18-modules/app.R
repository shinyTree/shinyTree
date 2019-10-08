library(shiny)
library(shinyTree)

## Tree Data + Function ############
elem <- structure("", sttype = "elem", sticon = "fa fa-file")
my_tree <- list(root1 = structure(list(elem1 = elem, elem2 = elem, elem3 = elem), 
                                  sttype = "root", sticon = "fa fa-archive"),
                root2 = structure(list(elem1 = elem, elem2 = elem, elem3 = elem), 
                                  sttype = "root", sticon = "fa fa-archive"),
                root3 = structure(list(elem1 = elem, elem2 = elem, elem3 = elem),
                                  sttype = "root", sticon = "fa fa-archive"))

get_part_tree <- function(scope) {
  part_tree <- my_tree[-sample(3, 1)]
  names(part_tree) <- paste(scope, names(part_tree), sep = "_")
  part_tree
}

## Module UI + Server Function ############
tree_module_ui <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns("go"), "Go!"),
    h4("Tree with Types"),
    shinyTree(ns("tree_with_type"))
  )
}
tree_module <- function(input, output, session) {
  output$tree_with_type <- renderTree({
    input$go
    get_part_tree("Module w/ Type")
  })
}

## UI + Server ############
ui <- fluidPage(
  h3("Module"),
  tree_module_ui("tree_module")
)
server <- function(input, output, session) {
  callModule(tree_module,
             "tree_module")
}

shinyApp(ui, server)