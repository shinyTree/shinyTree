library(shiny)
library(shinyTree)

#' Define UI for application that demonstrates a simple Tree editor
#' @author Mike Schaffer \email{mschaff@gmail.com}
shinyUI(
  pageWithSidebar(
    # Application title
    headerPanel("shinyTree with built-in themes"),
    
    sidebarPanel(
      helpText(HTML("An example of using shinyTree themes."))
    ),
    mainPanel(
      "Theme: default",
      shinyTree("tree", theme="default"),
      hr(),
      "Theme: default-dark",
      shinyTree("tree2", theme="default-dark"),
      hr(),
      "Theme: proton",
      shinyTree("tree3", theme="proton")
    )
  ))