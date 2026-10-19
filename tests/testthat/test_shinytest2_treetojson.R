library(shinyTree)
library(shinytest2)

test_that("get_selected works on a tree rendered from treeToJSON (issue #103)", {
  ## Adversarial end-to-end regression test for
  ## https://github.com/shinyTree/shinyTree/issues/103
  ##
  ## treeToJSON()'s state template covers opened/disabled/selected/loaded but
  ## not "checked". supplementAttr() tested each flag with `!= FALSE`, so the
  ## missing key made if() raise "argument is of length zero", the input handler
  ## errored, input$tree was never populated, and the req()-gated output stayed
  ## blank -- presenting as get_selected() returning an empty list.
  skip_on_cran()
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("data.tree")

  app <- tryCatch(
    AppDriver$new(
      app_dir = test_path("apps/test_treetojson"),
      name = "shinytree-treetojson",
      variant = NULL,
      load_timeout = 30000
    ),
    error = function(e) e
  )
  if (inherits(app, "error")) {
    skip(paste("AppDriver could not be started:", conditionMessage(app)))
  }
  withr::defer(app$stop())

  ## The payload really does omit "checked" -- otherwise this test proves nothing.
  state_keys <- jsonlite::fromJSON(app$get_js(
    "JSON.stringify(Object.keys($('#tree').jstree(true).get_json()[0].state))"
  ))
  expect_false("checked" %in% state_keys)

  ## Clicking a node must reach the server rather than erroring in the handler.
  app$click(selector = "[id=\"1_anchor\"]")
  app$wait_for_idle()

  expect_true(app$get_js("$('#tree').jstree(true).is_selected('1')"))

  tree <- app$get_value(input = "tree")
  expect_false(is.null(tree))
  sel <- get_selected(tree)
  expect_length(sel, 1)
  expect_equal(sel[[1]][1], "a")
  expect_true(attr(sel[[1]], "stselected"))

  ## And the rendered output reflects it.
  expect_equal(app$get_value(output = "sel"), "a")
})
