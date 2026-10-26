library(shiny)
library(shinyTree)

#' Example of creating a tree from a datatable and converting the tree back to datatable
shinyUI(
  pageWithSidebar(
    # Application title
    headerPanel("datatable to tree"),
    
    sidebarPanel(
      helpText(HTML("DataTable to ShinyTree and back.
                  <hr>Created using <a href = \"https://github.com/shinyTree/shinyTree\">shinyTree</a>."))
    ),
    mainPanel(
      # Show a simple table.
      shinyTree("tree"),
      tableOutput("df")
    )
  ))