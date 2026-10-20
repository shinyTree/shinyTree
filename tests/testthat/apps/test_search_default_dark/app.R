library(shiny)
library(shinyTree)

## One theme per app on purpose. Loading several themes on a single page lets
## one theme's stylesheet supply rules another omits, which masks exactly the
## bug this is testing (#113).
tree_data <- dfToTree(data.frame(Titanic), c("Sex", "Class", "Survived"))

ui <- fluidPage(
  shinyTree("tree", search = TRUE, theme = "default-dark")
)

server <- function(input, output, session) {
  output$tree <- renderTree(tree_data)
}

shinyApp(ui, server)
