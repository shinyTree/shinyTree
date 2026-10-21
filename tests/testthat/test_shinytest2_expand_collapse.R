library(shinytest2)

test_that("expandTree() and collapseTree() open and close every node (#42)", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")

  app <- tryCatch(
    AppDriver$new(
      app_dir = test_path("apps/test_expand_collapse"),
      name = "shinytree-expand-collapse",
      variant = NULL,
      load_timeout = 30000
    ),
    error = function(e) e
  )
  if (inherits(app, "error")) {
    skip(paste("AppDriver could not be started:", conditionMessage(app)))
  }
  withr::defer(app$stop())

  ## Parents in the client: their open state, keyed by label.
  open_parents <- function() {
    jsonlite::fromJSON(app$get_js(
      "(function(){var t=$('#tree').jstree(true), o={};
        Object.keys(t._model.data).forEach(function(id){
          if (id !== '#' && t.is_parent(id)) { o[t.get_node(id).text] = t.is_open(id); }
        }); return JSON.stringify(o); })()"
    ))
  }
  ## Parents reported open to the server.
  opened_in_r <- function() {
    walk <- function(x) {
      if (!is.list(x)) return(character())
      c(names(x)[vapply(x, function(n) isTRUE(attr(n, "stopened")), logical(1))],
        unlist(lapply(x, walk), use.names = FALSE))
    }
    sort(walk(app$get_value(input = "tree")))
  }
  checked_in_r <- function() {
    vapply(get_checked(app$get_value(input = "tree")), `[`, "", 1)
  }

  app$wait_for_js("$('#tree li').length > 0")
  app$wait_for_idle()
  expect_equal(unlist(open_parents()[c("root1", "root2")]), c(root1 = TRUE, root2 = FALSE))

  app$click("expand")
  app$wait_for_js("$('#tree').jstree(true).is_open($('#tree a:contains(c1)').closest('li').attr('id'))")
  app$wait_for_idle()
  expect_true(all(unlist(open_parents())))
  expect_equal(opened_in_r(), sort(c("root1", "a", "b", "root2", "c", "c1")))
  ## The tree was not rebuilt: the selection survives. b is checked too, since
  ## with three_state a parent is checked when all of its children are.
  expect_equal(checked_in_r(), c("b1", "b"))

  app$click("collapse")
  app$wait_for_js("$('#tree li:visible').length === 2")
  app$wait_for_idle()
  expect_false(any(unlist(open_parents())))
  expect_length(opened_in_r(), 0)
  expect_equal(checked_in_r(), c("b1", "b"))
})
