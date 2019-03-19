library(shiny)
library(shinyTree)
library(promises)
library(future)
plan(multiprocess)

dt <- list(
  root1 = structure("123",stinfo="A"),
  root2 = list(
    SubListA = list(leaf1 = structure("leaf1",stinfo="B"), leaf2 = "", leaf3=""),
    SubListB = list(leafA = "", leafB = "")
  )
)

#' Example of using promises to render a tree asynchronously
shinyServer(function(input, output, session) {
  tree_normal <- reactive({
    dt
  })
  output$tree <- renderTreeAsync({
    req(tree_normal())
    tree_normal()
  })
  
  
  tree_future <- reactive({
    future({
      dt
    })
  })
  output$tree_async <- renderTreeAsync({
    req(tree_future())
    tree_future()
  })
  
})
