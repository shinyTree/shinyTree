library(shiny)
library(shinyTree)

#' Define UI for an application demonstrating duplicate sibling node names
shinyUI(
  pageWithSidebar(
    # Application title
    headerPanel("shinyTree with duplicate node names"),

    sidebarPanel(
      helpText(HTML("Sibling nodes are allowed to share the same name. Here several
                  nodes are called <code>Invoice</code>, each carrying its own
                  <code>stinfo</code> attribute.
                  <br><br>Check a few boxes: <code>get_checked()</code> reports every
                  checked node individually, along with the attributes belonging to
                  that specific node rather than to the first or last sibling.
                  <br><br>This example uses <code>tie_selection = FALSE</code> so that
                  checking a node is independent of selecting it.
                  <hr>Created using <a href = \"http://github.com/shinyTree/shinyTree\">shinyTree</a>."))
    ),
    mainPanel(
      shinyTree("tree", checkbox = TRUE, tie_selection = FALSE),
      h4("get_checked()"),
      verbatimTextOutput("checked_info"),
      h4("Nodes received by the server"),
      verbatimTextOutput("node_count")
    )
  )
)
