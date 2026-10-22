library(shiny)
library(shinyTree)

#' Define server logic for trees that remember their state across reloads
shinyServer(function(input, output, session) {
  world <- list(
    Europe = structure(list(
      France = list(Paris = structure("", stselected = TRUE), Lyon = ""),
      Spain = list(Madrid = "", Seville = "")
    ), stopened = TRUE),
    Africa = list(
      Egypt = list(Cairo = ""),
      Morocco = list(Rabat = "", Fez = "")
    ),
    Asia = list(
      Japan = list(Tokyo = "", Osaka = ""),
      India = list(Delhi = "")
    )
  )
  output$saved <- renderTree(world)
  output$code <- renderTree(world)

  output$selected <- renderPrint({
    tree <- req(input$saved)
    cat(vapply(get_selected(tree), function(n) paste(c(attr(n, "ancestry"), n), collapse = " / "), ""),
        sep = "\n")
  })
})
