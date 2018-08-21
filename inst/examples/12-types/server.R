library(shiny)
library(shinyTree)

#' Examples of using jstree types to define node attributes
#' @author Michael Bell \email{bellma@@lilly.com}
shinyServer(function(input, output, session) {
  log <- c(paste0(Sys.time(), ": Interact with the tree to see the logs here..."))
  
  treeData <- reactive({
    list(
      root1 = structure("", stselected=TRUE,sttype="root"),
      root2 = structure(list(
        SubListA = structure(list(
            leaf1 = structure("",sttype="file"), 
            leaf2 = structure("",sttype="file"),
            leaf3 = structure("",sttype="file")),
            sttype="root",stopened=TRUE
            ),
        SubListB = structure(list(
          leafA = structure("",sttype="file"),
          leafB = structure("",sttype="file")
          ),stopened=TRUE,sttype="root")
      ),
      sttype="root",stopened=TRUE
    )
  )
  })
  
  observeEvent(input$updateTree,{
    updateTree(session, treeId = "tree", data = treeData())
  })
  
  output$tree <- renderTree({
    treeData()
  })
})
