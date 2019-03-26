library(shiny)
library(shinyTree)

#' Define server logic required to generate a simple tree
#' @author Jeff Allen \email{jeff@@trestletech.com}
shinyServer(function(input, output, session) {
  log <- c(paste0(Sys.time(), ": Interact with the tree to see the logs here..."))
  
  output$tree <- renderTree({
    list(
      root1 = structure("123"),
      root2 = list(
        SubListA = list(leaf1 = "", leaf2 = "", leaf3=""),
        SubListB = list(leafA = "", leafB = "")
      )
    )
  })
  
  output$selTxt <- renderText({
    tree <- input$tree
    if (is.null(tree)){
      "None"
    } else{
      unlist(get_selected(tree))
    }
  })
})