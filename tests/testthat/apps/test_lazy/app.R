library(shiny)
library(shinyTree)

## A lazily loaded tree (#11) for the browser tests. Settings come from the URL:
##   ?delay=<seconds>   children of non-root nodes take this long
##   ?searchfn=1        use a search function, which answers at once, rather
##                      than walking the tree with the (slow) children function
## Each level is named after its parent: g<gen>_1..3, then g<gen>_1.1..3, ...
## "dup" appears twice at the top level with different stids, and its
## children say which one they belong to. A "Rebuild" button bumps the
## generation, rebuilding the tree with different names.
ui <- function(req) {
  fluidPage(
    actionButton("rebuild", "Rebuild"),
    shinyTree("tree", checkbox = TRUE, search = TRUE, state = TRUE,
              contextmenu = FALSE, dragAndDrop = TRUE),
    verbatimTextOutput("report"),
    verbatimTextOutput("lazy_error")
  )
}

server <- function(input, output, session) {
  calls <- 0
  q <- isolate(parseQueryString(session$clientData$url_search))
  delay <- if (is.null(q$delay)) 0 else as.numeric(q$delay)

  use_search_fn <- identical(q$searchfn, "1")

  output$tree <- renderTree(lazy = TRUE, {
    gen <- input$rebuild + 1
    children <- function(path, node) {
      calls <<- calls + 1
      d <- length(path)
      if (d && delay > 0) Sys.sleep(delay)
      if (d == 0) {
        kids <- stats::setNames(lapply(1:3, function(i) structure("", stchildren = TRUE)),
                                paste0("g", gen, "_", 1:3))
        return(c(kids, list(dup = structure("", stchildren = TRUE, stid = 1),
                            dup = structure("", stchildren = TRUE, stid = 2))))
      }
      if (identical(path, "dup")) {
        ## "shared" is under both dups, for a search with two matches.
        return(c(stats::setNames(list(""), paste0("child of dup ", node$stid)),
                 list(shared = "")))
      }
      ## A name holding the \u0001 that state uses inside its saved paths,
      ## next to the usual children so other tests are unaffected.
      if (identical(path, "g1_3")) {
        kids <- stats::setNames(lapply(1:3, function(i) structure("", stchildren = TRUE)),
                                paste0("g1_3.", 1:3))
        return(c(kids, stats::setNames(list(structure("", stchildren = TRUE)), "ctlx")))
      }
      if (identical(path, c("g1_3", "ctlx"))) {
        return(list(inner = "", other = ""))
      }
      if (d >= 4) return(list())
      stats::setNames(lapply(1:3, function(i) structure("", stchildren = TRUE)),
                      paste0(path[d], ".", 1:3))
    }
    if (!use_search_fn) return(children)
    ## Names spell out their ancestry ("g1_3.2.1" is under "g1_3.2"), so the
    ## path to a name can be worked out without walking the tree.
    search <- function(text) {
      parts <- strsplit(text, ".", fixed = TRUE)[[1]]
      list(vapply(seq_along(parts), function(i) paste(parts[seq_len(i)], collapse = "."), ""))
    }
    list(children = children, search = search)
  })

  output$report <- renderPrint({
    tree <- req(input$tree)
    path <- function(n) paste(c(attr(n, "ancestry"), n), collapse = "/")
    cat("selected:", vapply(get_selected(tree), path, ""), "\n")
  })
  output$lazy_error <- renderPrint({
    e <- req(input$tree_lazy_error)
    cat("lazy error:", paste(unlist(e$path), collapse = "/"), "-", e$message)
  })
}

shinyApp(ui, server)
