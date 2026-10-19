library(shinyTree)
library(shinytest2)

test_that("selecting a node and reacting to an updateTree call works end-to-end", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")

  app <- tryCatch(
    AppDriver$new(
      app_dir = test_path("apps/test_selec"),
      name = "shinytree-select",
      variant = NULL,
      load_timeout = 30000
    ),
    error = function(e) e
  )
  if (inherits(app, "error")) {
    skip(paste("AppDriver could not be started:", conditionMessage(app)))
  }
  withr::defer(app$stop())

  ## Initial state: root1 is selected by the app's initial data
  vals <- app$get_values()
  expect_equal(as.numeric(vals$input$action), 0)
  expect_equal(
    vals$output$sel_names,
    paste0("[[1]]\n[1] \"root1\"\nattr(,\"ancestry\")\ncharacter(0)\n",
           "attr(,\"stselected\")\n[1] TRUE\nattr(,\"stchecked\")\n[1] FALSE\n")
  )
  expect_equal(vals$output$sel_classid, "[[1]]\n[1] \"root1\"\nattr(,\"id\")\n[1] \"1\"\n")
  expect_equal(vals$output$sel_slices, "[[1]]\n[[1]]$root1\n[1] 0\n\n")

  ## Clicking the action button triggers an updateTree() call on the server,
  ## which selects leaf1 (nested under root2/SubListA) instead.
  app$click(input = "action")
  app$wait_for_idle()

  vals <- app$get_values(output = c("sel_names", "sel_classid", "sel_slices"), input = "action")
  expect_equal(as.numeric(vals$input$action), 1)
  expect_equal(
    vals$output$sel_names,
    paste0("[[1]]\n[1] \"leaf1\"\nattr(,\"ancestry\")\n[1] \"root2\"    \"SubListA\"\n",
           "attr(,\"stselected\")\n[1] TRUE\nattr(,\"stchecked\")\n[1] FALSE\n")
  )
  expect_equal(vals$output$sel_classid, "[[1]]\n[1] \"leaf1\"\nattr(,\"id\")\n[1] \"4\"\n")
  expect_equal(
    vals$output$sel_slices,
    "[[1]]\n[[1]]$root2\n[[1]]$root2$SubListA\n[[1]]$root2$SubListA$leaf1\n[1] 0\n\n\n\n"
  )
})

test_that("clicking a node's checkbox in the browser updates input$tree", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")

  app <- tryCatch(
    AppDriver$new(
      app_dir = test_path("apps/test_checkbox"),
      name = "shinytree-checkbox",
      variant = NULL,
      load_timeout = 30000
    ),
    error = function(e) e
  )
  if (inherits(app, "error")) {
    skip(paste("AppDriver could not be started:", conditionMessage(app)))
  }
  withr::defer(app$stop())

  ## Nothing is selected yet.
  expect_null(app$get_value(input = "tree"))

  ## Click the checkbox icon next to "root1" (jsTree node id "1"). This
  ## exercises the client-side jQuery/jsTree input binding in shinyTree.js,
  ## which cannot be reached from R-only unit tests.
  app$click(selector = "[id=\"1_anchor\"] .jstree-checkbox")
  app$wait_for_idle()

  tree <- app$get_value(input = "tree")
  sel <- get_selected(tree)
  expect_length(sel, 1)
  expect_equal(sel[[1]][1], "root1")
  expect_true(attr(sel[[1]], "stselected"))
})
