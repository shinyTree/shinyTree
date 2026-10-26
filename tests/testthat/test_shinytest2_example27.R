library(shinyTree)
library(shinytest2)

test_that("the 27-lazy example loads, searches and fetches only what it needs (#11)", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")

  example_dir <- system.file("examples/27-lazy", package = "shinyTree")
  skip_if(example_dir == "", "27-lazy example not installed")

  app <- start_app(example_dir, "example-27-lazy")
  withr::defer(app$stop())

  app$run_js("localStorage.clear()")
  app$wait_for_js("$('#numbers li').length === 10", timeout = 15000)
  expect_equal(app$get_js("$('#numbers > ul > li > a').first().text()"), "1-100000")

  ## A search for a number six levels down fetches only the branches leading to it.
  app$run_js("$('#numbers-search-input').val('765432').trigger('keyup')")
  app$wait_for_js("Array.from(document.querySelectorAll('#numbers .jstree-search')).some(function(a){ return a.textContent === '765432'; })",
                  timeout = 15000)
  app$wait_for_idle()
  fetched <- as.numeric(sub(" of.*", "", app$get_value(output = "numbers_fetched")))
  expect_lte(fetched, 70)

  ## The folder tree lists the installed package.
  app$run_js("$('a[data-value=\"Files\"]').tab('show')")
  app$wait_for_js("$('#files li').length > 0", timeout = 15000)
  top <- jsonlite::fromJSON(app$get_js(
    "JSON.stringify(Array.from(document.querySelectorAll('#files > ul > li > a')).map(function(a){ return a.textContent; }))"))
  expect_true(all(c("examples", "www", "DESCRIPTION") %in% top))
})
