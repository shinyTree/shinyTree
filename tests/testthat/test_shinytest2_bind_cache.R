library(shinytest2)

test_that("renderTree works inside bindCache (issue #109)", {
  ## The tree is sent as a message rather than returned as the output value, so
  ## bindCache() refused it outright, and caching the NULL value would have left
  ## the tree empty on every cache hit.
  skip_on_cran()
  skip_if_not_installed("shinytest2")

  app <- start_app(test_path("apps/test_bind_cache"), "shinytree-bind-cache")
  withr::defer(app$stop())

  root <- function() {
    app$get_js("$('#tree .jstree-anchor').first().text()")
  }
  show_key <- function(key, expected_root) {
    app$set_inputs(key = key)
    app$wait_for_js(sprintf(
      "$('#tree .jstree-anchor').first().text() === '%s'", expected_root
    ))
    app$wait_for_idle()
  }

  app$wait_for_js("$('#tree li').length > 0")
  expect_equal(root(), "tree_a")
  expect_equal(app$get_value(output = "render_count"), "1")

  ## A new key renders.
  show_key("b", "tree_b")
  expect_equal(app$get_value(output = "render_count"), "2")

  ## Keys seen before are served from the cache without re-rendering, and the
  ## cached tree still reaches the client.
  show_key("a", "tree_a")
  show_key("b", "tree_b")
  expect_equal(app$get_value(output = "render_count"), "2")

  ## The tree is still a working input after a cache hit.
  app$click(selector = "#tree .jstree-anchor")
  app$wait_for_idle()
  expect_equal(get_selected(app$get_value(input = "tree"))[[1]][1], "tree_b")
})
