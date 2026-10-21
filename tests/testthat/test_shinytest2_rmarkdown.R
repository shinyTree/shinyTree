library(shinytest2)

test_that("a tree renders in an R Markdown document with runtime: shiny (#18)", {
  ## R Markdown drops tags written into <head>, so when shinyTree put its
  ## scripts there jsTree never loaded and the tree stayed empty.
  skip_on_cran()
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("rmarkdown")
  skip_if_not(rmarkdown::pandoc_available(), "pandoc is not available")

  app <- tryCatch(
    AppDriver$new(
      app_dir = test_path("apps/test_rmarkdown"),
      name = "shinytree-rmarkdown",
      variant = NULL,
      load_timeout = 60000
    ),
    error = function(e) e
  )
  if (inherits(app, "error")) {
    skip(paste("AppDriver could not be started:", conditionMessage(app)))
  }
  withr::defer(app$stop())

  app$wait_for_js("$('#tree li').length > 0", timeout = 20000)
  expect_equal(app$get_js("$('#tree li').length"), 2)

  ## The tree is a working input too. Wait for the selection to register in
  ## jsTree and then to reach the server: under covr in CI the input can lag
  ## behind wait_for_idle().
  app$click(selector = "#tree > ul > li:first-child > a.jstree-anchor")
  app$wait_for_js("$('#tree').jstree(true).get_selected().length > 0")
  selected <- list()
  for (i in 1:50) {
    selected <- get_selected(app$get_value(input = "tree"))
    if (length(selected) > 0) break
    Sys.sleep(0.2)
  }
  expect_length(selected, 1)
  expect_equal(selected[[1]][1], "root1")
})
