library(shinyTree)
library(shinytest2)

test_that("the 27-lazy example loads, searches and fetches only what it needs (#11)", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")

  example_dir <- system.file("examples/27-lazy", package = "shinyTree")
  skip_if(example_dir == "", "27-lazy example not installed")

  app <- start_app(example_dir, "example-27-lazy")
  withr::defer(app$stop())

  app$run_js("localStorage.clear()")
  app$wait_for_js("$('#numbers li').length === 10", timeout = 15000)
  expect_equal(app$get_js("$('#numbers > ul > li > a').first().text()"), "1-100000")

  ## A search for a number six levels down fetches only the branches leading to it.
  app$run_js("$('#numbers-search-input').val('765432').trigger('keyup')")
  app$wait_for_js("Array.from(document.querySelectorAll('#numbers .jstree-search')).some(function(a){ return a.textContent === '765432'; })",
                  timeout = 15000)
  app$wait_for_idle()
  fetched <- as.numeric(sub(" of.*", "", app$get_value(output = "numbers_fetched")))
  expect_lte(fetched, 70)

  ## The folder tree lists the installed package.
  app$run_js("$('a[data-value=\"Files\"]').tab('show')")
  app$wait_for_js("$('#files li').length > 0", timeout = 15000)
  top <- jsonlite::fromJSON(app$get_js(
    "JSON.stringify(Array.from(document.querySelectorAll('#files > ul > li > a')).map(function(a){ return a.textContent; }))"))
  expect_true(all(c("examples", "www", "DESCRIPTION") %in% top))
})

test_that("27-lazy: a branch is fetched when it is opened, and the tree is an input", {
  app <- example_app("27-lazy")
  withr::defer(app$stop())
  ## A new browser profile, so no state is remembered: only the top level is
  ## fetched to start with.
  ex_wait(app, "stTop('numbers').length === 10")
  wait_output(app, "numbers_fetched", "^10 of")
  ## Not fetched until opened: the first group has no children yet.
  expect_false(ex_js(app, "stTree('numbers').is_loaded(stNode('numbers', '1-100000'))"))

  ex_js(app, "stTree('numbers').open_node(stNode('numbers', '1-100000'))")
  ex_wait(app, "stKids('numbers', '1-100000').length === 10")
  expect_equal(ex_js(app, "stKids('numbers', '1-100000')[0]"), "1-10000")
  wait_output(app, "numbers_fetched", "^20 of")

  ## Opening down to a leaf and ticking it reaches the server.
  for (node in c("1-10000", "1-1000", "1-100", "1-10")) {
    ex_js(app, sprintf("stTree('numbers').open_node(stNode('numbers', '%s'))", node))
    ex_wait(app, sprintf("stKids('numbers', '%s').length === 10", node))
  }
  ex_wait(app, "stAnchor('numbers', '7') !== null")
  app$click(selector = node_checkbox(app, "numbers", "7"))
  expect_match(wait_output(app, "numbers_selected", "^7\\s*$"), "7")

  ## The folder tree: a folder's children are its files.
  app$run_js("$('a[data-value=\"Files\"]').tab('show')")
  ex_wait(app, "stNode('files', 'examples') !== null")
  ex_js(app, "stTree('files').open_node(stNode('files', 'examples'))")
  ex_wait(app, "stKids('files', 'examples').indexOf('27-lazy') >= 0")
  app$click(selector = node_anchor(app, "files", "DESCRIPTION"))
  expect_match(wait_output(app, "files_selected", "DESCRIPTION"), "^DESCRIPTION")
  expect_no_js_errors(app)
})
