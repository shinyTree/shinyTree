library(shiny)
library(shinyTree)

#' Define UI for application that demonstrates a simple Tree editor
#' @author Jeff Allen \email{jeff@@trestletech.com}
shinyUI(
  pageWithSidebar(
    # Application title
    headerPanel("shinyTree contextmenu!"),
    
    sidebarPanel(
      helpText(HTML("A simple Shiny Tree example using the contextmenu plugin.
                    <hr>Created using <a href = \"http://github.com/trestletech/shinyTree\">shinyTree</a>."))
    ),
    mainPanel(
      # Show a simple table with contextmenu.
      shinyTree("tree", contextmenu = TRUE, search = TRUE, unique = TRUE, sort = TRUE),
      
      verbatimTextOutput("sel_names"),
      verbatimTextOutput("sel_slices"),
      verbatimTextOutput("sel_classid")
    )
  ))