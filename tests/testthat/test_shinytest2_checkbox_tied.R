library(shinyTree)
library(shinytest2)

## Adversarial regression test for the remaining half of
## https://github.com/shinyTree/shinyTree/issues/121
##
## jsTree defines checked-ness in terms of selection when tie_selection is
## true: is_checked() *is* is_selected() and get_checked() *is* get_selected()
## (jstree.js, checkbox plugin). The node state carried by get_json(), which
## shinyTree transmits, nevertheless reports checked: false for a node that
## jsTree itself considers checked. shinyTree read that raw state and never
## consulted is_checked(), so with the default tie_selection = TRUE a user
## could tick a checkbox and have get_checked() return nothing.

test_that("get_checked reflects ticked boxes with the default tie_selection = TRUE (#121)", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")

  app <- tryCatch(
    AppDriver$new(
      app_dir = test_path("apps/test_checkbox_tied"),
      name = "shinytree-checkbox-tied",
      variant = NULL,
      load_timeout = 30000
    ),
    error = function(e) e
  )
  if (inherits(app, "error")) {
    skip(paste("AppDriver could not be started:", conditionMessage(app)))
  }
  withr::defer(app$stop())

  ## Sanity check: this app really does use the tied default.
  expect_equal(app$get_js("document.getElementById('tree').dataset.stTieSelection"), "TRUE")
  expect_null(app$get_value(input = "tree"))

  ## Tick the checkbox next to "root1".
  app$click(selector = "[id=\"1_anchor\"] .jstree-checkbox")
  app$wait_for_idle()

  ## jsTree considers the node checked ...
  expect_true(app$get_js("$('#tree').jstree(true).is_checked('1')"))

  ## ... so get_checked() must report it too.
  tree <- app$get_value(input = "tree")
  chk <- get_checked(tree)
  expect_length(chk, 1)
  expect_equal(chk[[1]][1], "root1")

  ## In tied mode checked and selected describe the same thing.
  sel <- get_selected(tree)
  expect_length(sel, 1)
  expect_equal(sel[[1]][1], "root1")

  ## Clicking the label (jsTree's whole_node path) must behave the same way.
  app$click(selector = "[id=\"3_anchor\"]")
  app$wait_for_idle()
  checked_names <- vapply(get_checked(app$get_value(input = "tree")), function(n) n[1], character(1))
  expect_true("leaf1" %in% checked_names)

  ## Unticking removes it again.
  app$click(selector = "[id=\"3_anchor\"]")
  app$wait_for_idle()
  checked_names <- vapply(get_checked(app$get_value(input = "tree")), function(n) n[1], character(1))
  expect_false("leaf1" %in% checked_names)
})

test_that("tie_selection = FALSE keeps checked and selected independent (#121/#122)", {
  ## The other half of the issue, already fixed in #133: with tie_selection
  ## false a node can be checked without being selected. Guards against the
  ## tied-mode fix above collapsing the two in untied mode.
  skip_on_cran()
  skip_if_not_installed("shinytest2")

  app <- tryCatch(
    AppDriver$new(
      app_dir = test_path("apps/test_checkbox_untied"),
      name = "shinytree-checkbox-untied-independence",
      variant = NULL,
      load_timeout = 30000
    ),
    error = function(e) e
  )
  if (inherits(app, "error")) {
    skip(paste("AppDriver could not be started:", conditionMessage(app)))
  }
  withr::defer(app$stop())

  expect_equal(app$get_js("document.getElementById('tree').dataset.stTieSelection"), "FALSE")

  app$click(selector = "[id=\"1_anchor\"] .jstree-checkbox")
  app$wait_for_idle()

  tree <- app$get_value(input = "tree")
  expect_length(get_checked(tree), 1)
  ## Checking must not imply selection when the two are untied.
  expect_length(get_selected(tree), 0)
})
