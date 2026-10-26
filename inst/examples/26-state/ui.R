library(shiny)
library(shinyTree)

#' Define UI for an application whose tree remembers its state across reloads
shinyUI(
  pageWithSidebar(
    headerPanel("shinyTree that remembers its state"),

    sidebarPanel(
      helpText(HTML("Open some nodes, select or tick a few, then reload the page:
                    the tree comes back the way you left it.
                    <br><br>The left tree uses <code>state = TRUE</code>, so what you
                    did wins over the app's own <code>stopened</code> and
                    <code>stselected</code> (here <em>Europe</em> and <em>Paris</em>).
                    The right tree adds <code>state_priority = \"code\"</code>, so
                    those stay open and selected and the rest of what you did is
                    restored around them.
                    <br><br>The state is kept in this browser only.
                    <hr>Created using <a href = \"https://github.com/shinyTree/shinyTree\">shinyTree</a>."))
    ),
    mainPanel(
      fluidRow(
        column(6, h4("state = TRUE"),
               shinyTree("saved", checkbox = TRUE, state = TRUE)),
        column(6, h4("state_priority = \"code\""),
               shinyTree("code", checkbox = TRUE, state = TRUE, state_priority = "code"))
      ),
      hr(),
      "Selected (left tree):",
      verbatimTextOutput("selected")
    )
  )
)
