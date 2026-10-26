library(shinyTree)
library(shinytest2)

test_that("selecting a node and reacting to an updateTree call works end-to-end", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")

  app <- start_app(test_path("apps/test_selec"), "shinytree-select")
  withr::defer(app$stop())

  ## Initial state: root1 is selected by the app's initial data
  vals <- app$get_values()
  expect_equal(as.numeric(vals$input$action), 0)
  expect_equal(
    vals$output$sel_names,
    paste0("[[1]]\n[1] \"root1\"\nattr(,\"ancestry\")\ncharacter(0)\n",
           "attr(,\"stselected\")\n[1] TRUE\n")
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
           "attr(,\"stselected\")\n[1] TRUE\n")
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

  app <- start_app(test_path("apps/test_checkbox"), "shinytree-checkbox")
  withr::defer(app$stop())

  ## The tree is reported as soon as it loads, with nothing selected yet.
  expect_length(get_selected(app$get_value(input = "tree")), 0)

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

test_that("clicking a node's label (not its checkbox) also updates input$tree", {
  ## A user clicking the node text goes through jsTree's whole_node handling,
  ## a different path from clicking the .jstree-checkbox icon above. Both must
  ## reach Shiny.
  skip_on_cran()
  skip_if_not_installed("shinytest2")

  app <- start_app(test_path("apps/test_checkbox"), "shinytree-checkbox-label")
  withr::defer(app$stop())

  expect_length(get_selected(app$get_value(input = "tree")), 0)

  ## Click the label itself rather than the checkbox icon.
  app$click(selector = "[id=\"1_anchor\"]")
  app$wait_for_idle()

  sel <- get_selected(app$get_value(input = "tree"))
  expect_length(sel, 1)
  expect_equal(sel[[1]][1], "root1")
  expect_true(attr(sel[[1]], "stselected"))
})
