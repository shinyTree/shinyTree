library(shiny)
library(shinyTree)

#' Example of storing and retreiving data in the tree
shinyServer(function(input, output, session) {
  log <- c(paste0(Sys.time(), ": Interact with the tree to see the logs here..."))
  
  output$tree <- renderTree({
    list(
      root1 = structure("123",stinfo="A"),
      root2 = list(
        SubListA = list(leaf1 = structure("leaf1",stinfo="B"), leaf2 = "", leaf3=""),
        SubListB = list(leafA = "", leafB = "")
      )
    )
  })
  
  output$selTxt <- renderText({
    tree <- input$tree
    if (is.null(tree)){
      "None"
    } else{
      unlist(lapply(get_selected(tree),function(node){
        paste("Info for",node,":",attr(node, "stinfo"))
      }))
    }
  })
})