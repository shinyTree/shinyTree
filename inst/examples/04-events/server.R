library(shiny)
library(shinyAce)

#' Define server logic required to generate a simple tree
#' @author Jeff Allen \email{jeff@@trestletech.com}
shinyServer(function(input, output, session) {
  log <- c(paste0(Sys.time(), ": Interact with the tree to see the logs here..."))
  
  output$tree <- renderTree({
    l <- list()
    l[[1]] <- "abc"
    l[[2]] <- 123
    l
  })
  
  output$eventLog <- renderText({
    paste(log, collapse="\n")
  })
})