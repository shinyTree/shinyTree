library(shinytest2)

test_that("input$tree reflects three_state cascades when tie_selection = FALSE", {
  ## shinyTree's check_node/uncheck_node handlers are bound before jsTree's own,
  ## which is where three_state cascades a change to parents and children. Read
  ## synchronously, input$tree reported the state from before the cascade:
  ## checking a parent reported only the parent, and unchecking a child left its
  ## parent reported as checked.
  skip_on_cran()
  skip_if_not_installed("shinytest2")

  app <- start_app(test_path("apps/test_checkbox_untied_cascade"), "shinytree-checkbox-untied-cascade")
  withr::defer(app$stop())

  checked_in_r <- function() {
    sort(vapply(get_checked(app$get_value(input = "tree")), `[`, "", 1))
  }
  checked_in_jstree <- function() {
    sort(jsonlite::fromJSON(app$get_js(
      "JSON.stringify($('#tree').jstree(true).get_checked(true).map(function(n){return n.text;}))"
    )))
  }

  app$wait_for_js("$('#tree li').length > 0")

  ## Checking root2 cascades down to both leaves.
  app$click(selector = "[id=\"2_anchor\"] .jstree-checkbox")
  app$wait_for_js("$('#tree').jstree(true).is_checked('3') === true")
  app$wait_for_idle()
  expect_equal(checked_in_jstree(), c("leaf1", "leaf2", "root2"))
  expect_equal(checked_in_r(), checked_in_jstree())

  ## Unchecking leaf1 cascades up and unchecks root2.
  app$click(selector = "[id=\"3_anchor\"] .jstree-checkbox")
  app$wait_for_js("$('#tree').jstree(true).is_checked('2') === false")
  app$wait_for_idle()
  expect_equal(checked_in_jstree(), "leaf2")
  expect_equal(checked_in_r(), checked_in_jstree())
})
