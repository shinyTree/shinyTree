library(shinyTree)
library(shinytest2)

test_that("the 04-selected example contrasts selected and checked in each mode", {
  ## Guards inst/examples/04-selected against rot, and pins the behaviour it
  ## documents: tied, checked and selected agree; untied, they are independent.
  skip_on_cran()
  skip_if_not_installed("shinytest2")

  example_dir <- system.file("examples/04-selected", package = "shinyTree")
  skip_if(example_dir == "", "04-selected example not installed")

  app <- start_app(example_dir, "example-04-selected")
  withr::defer(app$stop())

  reported <- function(fun) {
    app$wait_for_idle()
    sort(vapply(fun(app$get_value(input = "tree123")), `[`, "", 1))
  }
  wait_for_tree <- function() {
    app$wait_for_js("$('#tree123 li').length > 0")
    app$wait_for_idle()
  }
  wait_for_tree()

  ## Tied (the default): clicking a label both selects and checks it.
  app$click(selector = "[id=\"1_anchor\"]")
  expect_equal(reported(get_selected), "root1")
  expect_equal(reported(get_checked), "root1")

  ## Untied, whole_node off: the label selects, the box checks, independently.
  app$set_inputs(tie_selection = FALSE, whole_node = FALSE)
  wait_for_tree()
  app$click(selector = "[id=\"1_anchor\"]")
  app$click(selector = "[id=\"3_anchor\"] .jstree-checkbox")
  expect_equal(reported(get_selected), "root1")
  expect_equal(reported(get_checked),
               sort(c("SubListA", "leaf1", "leaf2", "leaf3")))

  ## No checkboxes: nothing can be checked.
  app$set_inputs(checkbox = FALSE)
  wait_for_tree()
  expect_equal(app$get_js("$('#tree123 .jstree-checkbox').length"), 0)
  app$click(selector = "[id=\"1_anchor\"]")
  expect_equal(reported(get_selected), "root1")
  expect_length(reported(get_checked), 0)
})
