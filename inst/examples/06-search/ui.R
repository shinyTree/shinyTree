library(shiny)
library(shinyTree)

#' Define UI for application that demonstrates a simple Tree editor
#' @author Jeff Allen \email{jeff@@trestletech.com}
shinyUI(
  pageWithSidebar(
    # Application title
    headerPanel("Searchable shinyTree!"),
    
    sidebarPanel(
      helpText(p(HTML("A searchable Shiny Tree example &mdash; try searching for a county in the text box on the right.")),
                 p(HTML("Try searching for <code>Mata</code>")),
                 p("Data was irreproducibly pulled from ", tags$a(href="http://www.countymapsoftexas.com/regionsall.shtml", "this page"), "; some rows might have been missed."),
                 HTML("<hr>Created using <a href = \"http://github.com/trestletech/shinyTree\">shinyTree</a>."))
    ),
    mainPanel(
      # Show a simple table.
      shinyTree("tree", search=TRUE, searchtime = 1000)
    )
  ))