library(shinytest2)

test_that("checking a box updates input$tree when tie_selection = FALSE (issue #122)", {
  ## Adversarial regression test for https://github.com/shinyTree/shinyTree/issues/122
  ## and PR https://github.com/shinyTree/shinyTree/pull/122.
  ##
  ## With checkbox = TRUE and tie_selection = FALSE, jsTree's checkbox plugin
  ## fires check_node.jstree/uncheck_node.jstree (not select_node.jstree), so
  ## clicking a checkbox alone should still update input$tree. On unpatched
  ## shinyTree.js, no Shiny input binding is subscribed to those events, so
  ## this currently reproduces as a hang: input$tree never updates and the
  ## output (gated behind req()) never renders.
  skip_on_cran()
  skip_if_not_installed("shinytest2")

  app <- start_app(test_path("apps/test_checkbox_untied"), "shinytree-checkbox-untied")
  withr::defer(app$stop())

  ## Nothing selected/checked yet.
  expect_length(get_checked(app$get_value(input = "tree")), 0)

  ## Click the checkbox icon next to "root1" (jsTree node id "1").
  app$click(selector = "[id=\"1_anchor\"] .jstree-checkbox")

  ## jsTree-side state must flip to checked regardless of the Shiny binding bug.
  app$wait_for_js("$('#tree').jstree(true).is_checked('1') === true")

  ## The real assertion: input$tree must reflect the checkbox change. On
  ## unpatched shinyTree.js this times out because no callback ever fires.
  app$wait_for_value(input = "tree", ignore = list(NULL), timeout = 5000)

  tree <- app$get_value(input = "tree")
  expect_false(is.null(tree))

  checked <- get_checked(tree)
  expect_length(checked, 1)
  expect_equal(checked[[1]][1], "root1")
  expect_true(attr(checked[[1]], "stchecked"))
})
