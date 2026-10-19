library(shinytest2)

test_that("right-clicking a node opens the jsTree contextmenu and Delete removes it", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")

  app <- tryCatch(
    AppDriver$new(
      app_dir = test_path("apps/test_contextmenu"),
      name = "shinytree-contextmenu",
      variant = NULL,
      load_timeout = 30000
    ),
    error = function(e) e
  )
  if (inherits(app, "error")) {
    skip(paste("AppDriver could not be started:", conditionMessage(app)))
  }
  withr::defer(app$stop())

  sess <- app$get_chromote_session()

  box <- sess$Runtime$evaluate(
    "JSON.stringify(document.getElementById('1_anchor').getBoundingClientRect())"
  )
  rect <- jsonlite::fromJSON(box$result$value)
  x <- rect$left + rect$width / 2
  y <- rect$top + rect$height / 2

  ## Right-click "root1" to open jsTree's native contextmenu.
  sess$Input$dispatchMouseEvent(type = "mousePressed", x = x, y = y, button = "right", clickCount = 1)
  sess$Input$dispatchMouseEvent(type = "mouseReleased", x = x, y = y, button = "right", clickCount = 1)
  app$wait_for_js("document.querySelector('.vakata-context') !== null")

  menu_html <- app$get_html("body")
  expect_true(grepl("vakata-context", menu_html))
  expect_true(grepl(">Delete<", menu_html))

  ## Click "Delete" in the contextmenu.
  app$run_js('document.querySelector("a[rel=\\"2\\"]").click();')
  app$wait_for_idle()

  tree <- app$get_value(input = "tree")
  expect_setequal(names(tree), "root2")
  expect_null(tree$root1)
})
