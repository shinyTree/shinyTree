library(shiny)
library(shinyTree)

## Reproduction from issue #129. The tree lives two Shiny module levels deep, so
## its id contains two hyphens ("another_module-tree_module-tree_with_type").
## The R side names the types global with gsub(), replacing both; the client used
## String.replace with a string argument, which replaces only the first, so the
## lookup missed and the types plugin failed on a null settings object.
elem <- structure("", sttype = "elem")
my_tree <- list(root1 = structure(list(elem1 = elem, elem2 = elem, elem3 = elem),
                                  sttype = "root"),
                root2 = structure(list(elem1 = elem, elem2 = elem, elem3 = elem),
                                  sttype = "root"))

tree_module_ui <- function(id) {
    ns <- NS(id)
    tagList(
        h4("Tree with Types"),
        shinyTree(
            ns("tree_with_type"),
            types = "{'root': {'icon': 'fa fa-archive'}, 'elem':{'icon': 'fa fa-file'}}"
        )
    )
}
tree_module <- function(input, output, session) {
    output$tree_with_type <- renderTree(my_tree)
}

another_module_ui <- function(id) {
    ns <- NS(id)
    tree_module_ui(ns("tree_module"))
}

another_module <- function(input, output, session) {
    callModule(tree_module, "tree_module")
}

ui <- fluidPage(
    h3("Tree Module in another module"),
    another_module_ui("another_module")
)

server <- function(input, output, session) {
    callModule(another_module, "another_module")
}

shinyApp(ui, server)
