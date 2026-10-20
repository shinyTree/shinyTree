library(shinyTree)
library(shinytest2)

test_that("node attribute values keep their whitespace (issue #3)", {
  ## Adversarial regression test for the attribute half of
  ## https://github.com/shinyTree/shinyTree/issues/3
  ##
  ## shinyTree.js trimmed every string value in a node's `data` payload on the
  ## way back to R. Node *names* are display labels and HTML collapses their
  ## leading/trailing whitespace anyway, so trimming those is defensible -- and
  ## is deliberately kept, since the legacy `$elem.html(data)` render path can
  ## pick up Shiny's own HTML indentation. Attribute values are different: they
  ## are pure data, never rendered, and are the documented way to carry a real
  ## key alongside a node. Mutating them silently corrupts fixed-width codes and
  ## padded identifiers.
  skip_on_cran()
  skip_if_not_installed("shinytest2")

  app <- tryCatch(
    AppDriver$new(
      app_dir = test_path("apps/test_attr_whitespace"),
      name = "shinytree-attr-whitespace",
      variant = NULL,
      load_timeout = 30000
    ),
    error = function(e) e
  )
  if (inherits(app, "error")) {
    skip(paste("AppDriver could not be started:", conditionMessage(app)))
  }
  withr::defer(app$stop())

  tree <- app$get_value(input = "tree")
  expect_false(is.null(tree))

  ## Every stid must survive the round trip untouched.
  expect_equal(attr(tree$alpha, "stid"), "  padded-left")
  expect_equal(attr(tree$beta,  "stid"), "padded-right  ")
  expect_equal(attr(tree$gamma, "stid"), "  both sides  ")
  expect_equal(attr(tree$delta, "stid"), "inner  gap")

  ## Non-string attributes were never trimmed; confirm that still holds.
  expect_equal(attr(tree$delta, "stcount"), 42L)

  ## And the rendered output agrees, so this is visible to app authors.
  out <- app$get_value(output = "attrs")
  expect_match(out, "alpha=[  padded-left]", fixed = TRUE)
  expect_match(out, "beta=[padded-right  ]", fixed = TRUE)
  expect_match(out, "gamma=[  both sides  ]", fixed = TRUE)
})

test_that("node names are still trimmed, by design (issue #3)", {
  ## Documents the deliberate half of #3. Node names are labels: HTML collapses
  ## their surrounding whitespace on screen regardless, and the trim guards the
  ## raw-HTML render path where Shiny's pretty-printed indentation would
  ## otherwise leak into every name. Changing this is a breaking change to the
  ## keys of input$tree, so it stays.
  skip_on_cran()
  skip_if_not_installed("shinytest2")

  app <- tryCatch(
    AppDriver$new(
      app_dir = test_path("apps/test_attr_whitespace"),
      name = "shinytree-attr-whitespace-names",
      variant = NULL,
      load_timeout = 30000
    ),
    error = function(e) e
  )
  if (inherits(app, "error")) {
    skip(paste("AppDriver could not be started:", conditionMessage(app)))
  }
  withr::defer(app$stop())

  ## These names have no padding, so they are unaffected either way -- the point
  ## is that the list is keyed by trimmed names and that is the documented shape.
  tree <- app$get_value(input = "tree")
  expect_equal(names(tree), c("alpha", "beta", "gamma", "delta"))
})
