library(shiny)
library(shinyTree)

#' Define UI for an application demonstrating lazily loaded trees
shinyUI(
  pageWithSidebar(
    headerPanel("shinyTree with lazy loading"),

    sidebarPanel(
      helpText(HTML("With <code>renderTree(lazy = TRUE)</code> the tree's nodes are
                    fetched from the server as they are opened, so a tree can be far
                    too large to send whole.
                    <br><br><b>Numbers</b> holds every number from 1 to 1,000,000,
                    grouped by hundred-thousands, ten-thousands and so on. Search for
                    any number: a <code>search</code> function works out where it is,
                    and only the branches leading to it are fetched.
                    <br><br><b>Files</b> browses the folder shinyTree is installed in,
                    one level at a time. Its search walks the folders with the same
                    function that lists them.
                    <br><br>Both trees remember what was open and selected
                    (<code>state = TRUE</code>): reload the page to see it.
                    <hr>Created using <a href = \"https://github.com/shinyTree/shinyTree\">shinyTree</a>."))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Numbers",
          shinyTree("numbers", checkbox = TRUE, search = TRUE, state = TRUE),
          hr(),
          "Selected:", verbatimTextOutput("numbers_selected"),
          "Nodes fetched so far:", verbatimTextOutput("numbers_fetched")
        ),
        tabPanel("Files",
          shinyTree("files", search = TRUE, state = TRUE),
          hr(),
          "Selected:", verbatimTextOutput("files_selected"),
          verbatimTextOutput("files_error")
        )
      )
    )
  )
)
