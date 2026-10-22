library(shiny)
library(shinyTree)

## Trees that remember their open/selected/checked nodes across page reloads
## (#19). Settings come from the URL so one app covers every case:
##   ?priority=saved|code   which wins over the rendered st* attributes
##   ?tied=0                checkboxes untied from the selection
## root2 is rendered opened and b1 selected/checked, so the "code" priority has
## something to keep. Two leaves share a name to check paths, not names, match.
ui <- function(req) {
  q <- parseQueryString(req$QUERY_STRING)
  priority <- if (identical(q$priority, "code")) "code" else "saved"
  tied <- !identical(q$tied, "0")
  fluidPage(
    shinyTree("tree", checkbox = TRUE, tie_selection = tied,
              state = TRUE, state_priority = priority),
    verbatimTextOutput("report")
  )
}

server <- function(input, output, session) {
  output$tree <- renderTree({
    list(
      root1 = list(a = list(a1 = "", dup = ""), b = list(dup = "")),
      root2 = structure(list(
        b1 = structure("", stselected = TRUE, stchecked = TRUE),
        b2 = ""
      ), stopened = TRUE)
    )
  })
  output$report <- renderPrint({
    tree <- req(input$tree)
    path <- function(n) paste(c(attr(n, "ancestry"), n), collapse = "/")
    cat("selected:", vapply(get_selected(tree), path, ""), "\n")
    cat("checked:", vapply(get_checked(tree), path, ""), "\n")
  })
}

shinyApp(ui, server)
