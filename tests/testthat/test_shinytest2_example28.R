library(shinyTree)
library(shinytest2)

test_that("the 28-ontology example shows the AEO and searches it (#33)", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")

  example_dir <- system.file("examples/28-ontology", package = "shinyTree")
  skip_if(example_dir == "", "28-ontology example not installed")

  app <- tryCatch(
    AppDriver$new(app_dir = example_dir, name = "example-28-ontology", variant = NULL,
                  load_timeout = 30000),
    error = function(e) e
  )
  if (inherits(app, "error")) {
    skip(paste("AppDriver could not be started:", conditionMessage(app)))
  }
  withr::defer(app$stop())

  app$wait_for_js("$('#tree li').length > 0", timeout = 15000)
  expect_equal(app$get_value(output = "summary"), "250 terms, 1 obsolete")
  top <- jsonlite::fromJSON(app$get_js(
    "JSON.stringify(Array.from(document.querySelectorAll('#tree > ul > li > a')).map(function(a){ return a.textContent; }))"))
  expect_equal(top, c("anatomical entity", "cell", "obsolete"))
  ## The definition is the hover text.
  expect_match(app$get_js("$('#tree > ul > li > a').first().attr('title')"), "^CARO:0000000: Biological entity")

  ## Search reaches a term several levels down, which is then selectable.
  app$run_js("$('#tree-search-input').val('lumen of epithelial sac').trigger('keyup')")
  app$wait_for_js("Array.from(document.querySelectorAll('#tree .jstree-search')).some(function(a){ return a.textContent === 'lumen of epithelial sac'; })",
                  timeout = 15000)
  app$run_js("var t = $('#tree').jstree(true); t.select_node($('#tree .jstree-search').first().closest('li').attr('id'));")
  app$wait_for_idle()
  expect_match(app$get_value(output = "selected"), "AEO:0000079 lumen of epithelial sac", fixed = TRUE)
})
