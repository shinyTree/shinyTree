library(shinytest2)

test_that("sttooltip shows as the node's hover text (#66)", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")

  app <- tryCatch(
    AppDriver$new(
      app_dir = test_path("apps/test_tooltip"),
      name = "shinytree-tooltip",
      variant = NULL,
      load_timeout = 30000
    ),
    error = function(e) e
  )
  if (inherits(app, "error")) {
    skip(paste("AppDriver could not be started:", conditionMessage(app)))
  }
  withr::defer(app$stop())

  titles <- function() {
    jsonlite::fromJSON(app$get_js(
      "JSON.stringify(Array.from(document.querySelectorAll('#tree a.jstree-anchor')).map(function(a){ return [a.textContent, a.getAttribute('title') || '']; }))"
    ))
  }

  app$wait_for_js("$('#tree li').length === 4")
  expect_equal(titles()[, 2], c("The first root", "", "Say \"hi\" <b>not bold</b>", ""))
  ## Shown as text, not markup.
  expect_equal(app$get_js("document.querySelectorAll('#tree b').length"), 0)

  ## jsTree redraws a node's anchor when its parent is closed and reopened.
  app$run_js("var t = $('#tree').jstree(true); t.close_all(); t.open_all();")
  app$wait_for_js("$('#tree li:visible').length === 4")
  expect_equal(titles()[3, 2], "Say \"hi\" <b>not bold</b>")
})
