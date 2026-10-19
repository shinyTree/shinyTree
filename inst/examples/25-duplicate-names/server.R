library(shiny)
library(shinyTree)

#' Define server logic demonstrating that duplicate sibling names are preserved
#' and that each duplicate keeps its own attributes.
shinyServer(function(input, output, session) {
  output$tree <- renderTree({
    list(
      `Q1 2026` = structure(
        list(
          Invoice = structure("", stinfo = "INV-001 / $1,200 / Acme"),
          Invoice = structure("", stinfo = "INV-002 / $3,450 / Globex"),
          Receipt = structure("", stinfo = "RCP-900 / $99 / Initech"),
          Invoice = structure("", stinfo = "INV-003 / $780 / Umbrella")
        ),
        stopened = TRUE
      ),
      `Q2 2026` = structure(
        list(
          Invoice = structure("", stinfo = "INV-004 / $5,000 / Stark"),
          Invoice = structure("", stinfo = "INV-005 / $2,310 / Wayne")
        ),
        stopened = TRUE
      )
    )
  })

  output$checked_info <- renderPrint({
    tree <- input$tree
    if (is.null(tree)) {
      cat("(nothing checked yet)")
      return(invisible())
    }
    checked <- get_checked(tree)
    if (!length(checked)) {
      cat("(nothing checked yet)")
      return(invisible())
    }
    for (node in checked) {
      cat(sprintf(
        "%-8s | %-8s | %s\n",
        node[1],
        paste(attr(node, "ancestry"), collapse = "/"),
        attr(node, "stinfo")
      ))
    }
  })

  # The duplicates survive the trip to the server: each quarter reports every
  # child, including the repeated names.
  output$node_count <- renderPrint({
    tree <- req(input$tree)
    for (quarter in names(tree)) {
      cat(quarter, "-", length(tree[[quarter]]), "children:",
          paste(names(tree[[quarter]]), collapse = ", "), "\n")
    }
  })
})
