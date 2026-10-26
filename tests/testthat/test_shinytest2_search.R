library(shinytest2)

type_into_search_box <- function(app, selector_id, text) {
  sess <- app$get_chromote_session()
  sess$Runtime$evaluate(sprintf("document.getElementById('%s').focus()", selector_id))
  for (ch in strsplit(text, "")[[1]]) {
    sess$Input$dispatchKeyEvent(type = "keyDown", text = ch)
    sess$Input$dispatchKeyEvent(type = "keyUp", text = ch)
  }
}

test_that("typing in the search box highlights matching nodes and hides the rest", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")

  app <- start_app(test_path("apps/test_search"), "shinytree-search")
  withr::defer(app$stop())

  ## All three leaves visible, none highlighted, before searching.
  html_before <- app$get_html("#tree")
  expect_false(grepl("jstree-search", html_before))
  expect_false(grepl("jstree-hidden", html_before))

  type_into_search_box(app, "tree-search-input", "ban")
  app$wait_for_js("document.querySelector('#tree .jstree-search') !== null")
  app$wait_for_idle()

  html_after <- app$get_html("#tree")

  ## "banana" matches and is highlighted with the jstree-search class ...
  banana_node <- regmatches(
    html_after,
    regexpr("<li[^>]*id=\"2\"[^>]*>.*?</li>", html_after, perl = TRUE)
  )
  expect_true(grepl("jstree-search", banana_node))
  expect_false(grepl("jstree-hidden", banana_node))

  ## ... while "apple" and "cherry" do not match and are hidden.
  apple_node <- regmatches(
    html_after,
    regexpr("<li[^>]*id=\"1\"[^>]*>.*?</li>", html_after, perl = TRUE)
  )
  cherry_node <- regmatches(
    html_after,
    regexpr("<li[^>]*id=\"3\"[^>]*>.*?</li>", html_after, perl = TRUE)
  )
  expect_true(grepl("jstree-hidden", apple_node))
  expect_true(grepl("jstree-hidden", cherry_node))
})
