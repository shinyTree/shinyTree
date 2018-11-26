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
            leaf1 = structure("",sttype="file",sticon="fa fa-signal"), 
            leaf2 = structure("",sttype="file"),
            leaf3 = structure("",sttype="file")),
            sttype="root",stopened=TRUE
            ),
        SubListB = structure(list(
          leafA = structure("",sttype="default",sticon="glyphicon glyphicon-leaf"),
          leafB = structure("",sttype="default",sticon="shinyTree/icon.png"),
          leafC = structure("",sttype="default",sticon="map-signs")
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
