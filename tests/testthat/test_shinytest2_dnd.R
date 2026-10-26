library(shinytest2)

test_that("dragging a node to a new position reorders the tree and updates input$tree", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")

  app <- start_app(test_path("apps/test_dnd"), "shinytree-dnd")
  withr::defer(app$stop())

  sess <- app$get_chromote_session()

  rect1 <- jsonlite::fromJSON(
    sess$Runtime$evaluate("JSON.stringify(document.getElementById('1_anchor').getBoundingClientRect())")$result$value
  )
  rect2 <- jsonlite::fromJSON(
    sess$Runtime$evaluate("JSON.stringify(document.getElementById('2_anchor').getBoundingClientRect())")$result$value
  )

  x <- rect1$left + 20
  y_start <- rect1$top + rect1$height / 2
  y_end <- rect2$bottom - 4

  ## Drag "root1" down past "root2" to reorder the two top-level nodes.
  ## jsTree's dnd plugin binds its own mousedown/mousemove/mouseup handlers
  ## (not HTML5 drag events), so a sequence of raw CDP mouse events is
  ## enough to trigger it -- the move only registers past a pixel threshold,
  ## hence the intermediate steps.
  sess$Input$dispatchMouseEvent(type = "mousePressed", x = x, y = y_start, button = "left", clickCount = 1)
  steps <- seq(y_start, y_end, length.out = 10)
  for (yy in steps) {
    sess$Input$dispatchMouseEvent(type = "mouseMoved", x = x, y = yy, button = "left")
  }
  app$wait_for_js("document.getElementById('jstree-dnd') !== null")
  sess$Input$dispatchMouseEvent(type = "mouseReleased", x = x, y = y_end, button = "left")
  app$wait_for_idle()

  tree <- app$get_value(input = "tree")
  expect_equal(names(tree), c("root2", "root1"))
})
