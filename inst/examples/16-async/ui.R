library(shiny)
library(shinyTree)

#' Example of using promises to render a tree asynchronously
shinyUI(
  pageWithSidebar(
    # Application title
    headerPanel("shinyTree data with 'async' rendering"),
    sidebarPanel(
      helpText(HTML("An example of using promises/future to render a tree asynchronously."))
    ),
    mainPanel(
      h4("Normal Tree"),
      shinyTree("tree"),
      h4("Async Tree"),
      shinyTree("tree_async")
    )
))
