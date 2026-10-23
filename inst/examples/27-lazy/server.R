library(shiny)
library(shinyTree)

#' Define server logic for lazily loaded trees
shinyServer(function(input, output, session) {

  ## --- Numbers: a million leaves, fetched a level at a time ----------------
  ## A node's path is its group at each level, e.g. c("100001-200000",
  ## "110001-120000", ...). Every group splits into ten; groups of ten split
  ## into single numbers, the leaves.
  group_name <- function(from, size) sprintf("%d-%d", from, from + size - 1)
  fetched <- reactiveVal(0)

  number_children <- function(path) {
    if (!length(path)) {
      from <- 1; size <- 1e6
    } else {
      bounds <- as.numeric(strsplit(path[length(path)], "-", fixed = TRUE)[[1]])
      from <- bounds[1]; size <- bounds[2] - bounds[1] + 1
    }
    step <- size / 10
    starts <- from + step * (0:9)
    isolate(fetched(fetched() + 10))
    if (step == 1) {
      # Single numbers: leaves.
      return(stats::setNames(rep(list(""), 10), format(starts, scientific = FALSE, trim = TRUE)))
    }
    stats::setNames(lapply(starts, function(s) structure("", stchildren = TRUE)),
                    vapply(starts, group_name, "", size = step))
  }

  ## The path to a number can be worked out without walking the tree, so search
  ## is instant however large the tree is.
  number_search <- function(text) {
    n <- suppressWarnings(as.numeric(gsub("[^0-9]", "", text)))
    if (is.na(n) || n < 1 || n > 1e6) return(list())
    path <- character(0)
    from <- 1; size <- 1e6
    while (size > 10) {
      size <- size / 10
      from <- from + size * floor((n - from) / size)
      path <- c(path, group_name(from, size))
    }
    list(c(path, format(n, scientific = FALSE, trim = TRUE)))
  }

  output$numbers <- renderTree(lazy = TRUE, {
    list(children = number_children, search = number_search)
  })
  output$numbers_selected <- renderPrint({
    tree <- req(input$numbers)
    cat(vapply(get_selected(tree, leaves_only = TRUE), `[`, "", 1), sep = "\n")
  })
  output$numbers_fetched <- renderPrint(cat(fetched(), "of 1,111,110"))

  ## --- Files: the folder shinyTree is installed in -------------------------
  root <- system.file(package = "shinyTree")

  file_children <- function(path) {
    dir <- do.call(file.path, as.list(c(root, path)))
    subdirs <- list.dirs(dir, full.names = FALSE, recursive = FALSE)
    # A symbolic link can point back up the tree; show it as a leaf.
    links <- nzchar(Sys.readlink(file.path(dir, subdirs)))
    files <- setdiff(list.files(dir), subdirs[!links])
    c(stats::setNames(lapply(subdirs[!links], function(d) structure("", stchildren = TRUE, sticon = "fa fa-folder")),
                      subdirs[!links]),
      stats::setNames(lapply(files, function(f) structure("", sticon = "fa fa-file")), files))
  }

  output$files <- renderTree(lazy = TRUE, file_children)
  output$files_selected <- renderPrint({
    tree <- req(input$files)
    cat(vapply(get_selected(tree), function(n) paste(c(attr(n, "ancestry"), n), collapse = "/"), ""),
        sep = "\n")
  })
  output$files_error <- renderPrint({
    e <- req(input$files_lazy_error)
    cat("Could not load", paste(unlist(e$path), collapse = "/"), ":", e$message)
  })
})
