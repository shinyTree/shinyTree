library(shinyTree)
library(shinytest2)

test_that("selecting a duplicate-named sibling reports that node's own data (issue #104)", {
  ## Adversarial end-to-end regression test for
  ## https://github.com/shinyTree/shinyTree/issues/104
  ##
  ## The app's SubListB holds four siblings all named "leafA". Clicking the
  ## second one must report *its* stinfo. Before the fix the named-list keying
  ## in jsonToAttr() collapsed the duplicates onto the last sibling, so this
  ## click produced empty output -- "not recognized to be selected except for
  ## the last entry", as reported.
  skip_on_cran()
  skip_if_not_installed("shinytest2")

  app <- start_app(test_path("apps/test_duplicates"), "shinytree-duplicates")
  withr::defer(app$stop())

  ## All five children render as distinct nodes, duplicates included.
  anchors <- app$get_js(
    "JSON.stringify(Array.from(document.querySelectorAll('#tree a.jstree-anchor')).map(function(a){return a.textContent;}))"
  )
  expect_equal(
    jsonlite::fromJSON(anchors),
    c("root1", "SubListB", "leafA", "leafA", "leafA", "leafB", "leafA")
  )

  ## Click the *second* leafA (jsTree node id 4) -- not the last sibling.
  app$click(selector = "[id=\"4_anchor\"]")
  app$wait_for_idle()
  expect_equal(app$get_value(output = "sel_info"), "leafA:Info LeafA2")

  ## The fourth leafA (id 7) must report its own data too.
  app$click(selector = "[id=\"7_anchor\"]")
  app$wait_for_idle()
  expect_equal(app$get_value(output = "sel_info"), "leafA:Info LeafA4")

  ## And the distinctly named sibling still behaves.
  app$click(selector = "[id=\"6_anchor\"]")
  app$wait_for_idle()
  expect_equal(app$get_value(output = "sel_info"), "leafB:Info LeafB")
})
