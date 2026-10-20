context("test_partial_state")

## Adversarial regression tests for issue #103:
## "get_selected after treeToJSON not working"
##
## supplementAttr() guarded each state attribute with `if (json$state$x != FALSE)`.
## When a key is absent, json$state$x is NULL, `NULL != FALSE` is logical(0), and
## if() raises "argument is of length zero". The input handler then errors, Shiny
## never populates input$tree, and req(tree) silently blanks the output -- which
## presents as get_selected() "returning an empty list".
##
## treeToJSON() hits this for real: its state template only covers
## opened/disabled/selected/loaded, so a data.tree node carrying a state slot
## produces a payload with no "checked" key at all.

make_state_node <- function(state, text = "a", id = "1") {
  list(text = text, children = list(), state = state, data = list(), id = id)
}

test_that("jsonToAttr handles a state slot with no checked key (treeToJSON shape)", {
  ## Exactly what treeToJSON emits: no "checked", plus the extra "loaded" flag.
  node <- make_state_node(list(loaded = TRUE, opened = FALSE,
                               selected = TRUE, disabled = FALSE))

  res <- shinyTree:::jsonToAttr(list(node))
  expect_is(res, "list")
  expect_equal(names(res), "a")
  expect_true(attr(res$a, "stselected"))
  expect_null(attr(res$a, "stchecked"))
})

test_that("get_selected works on a tree parsed from a treeToJSON-shaped payload", {
  node <- make_state_node(list(loaded = TRUE, opened = FALSE,
                               selected = TRUE, disabled = FALSE))
  tree <- shinyTree:::jsonToAttr(list(node))

  sel <- get_selected(tree)
  expect_length(sel, 1)
  expect_equal(sel[[1]][1], "a")
})

test_that("jsonToAttr tolerates any individual state key being absent", {
  ## Each key in turn is the only one missing.
  for (missing in c("selected", "checked", "disabled", "opened")) {
    full <- list(opened = FALSE, selected = FALSE, disabled = FALSE, checked = FALSE)
    full[[missing]] <- NULL
    res <- shinyTree:::jsonToAttr(list(make_state_node(full)))
    expect_is(res, "list")
    expect_equal(names(res), "a")
  }
})

test_that("jsonToAttr tolerates an entirely absent or empty state slot", {
  bare <- list(text = "a", children = list(), data = list(), id = "1")
  res <- shinyTree:::jsonToAttr(list(bare))
  expect_is(res, "list")
  expect_equal(names(res), "a")
  expect_null(attr(res$a, "stselected"))

  empty <- make_state_node(list())
  res2 <- shinyTree:::jsonToAttr(list(empty))
  expect_is(res2, "list")
  expect_equal(names(res2), "a")
})

test_that("partial state still records the flags that are present", {
  ## Only the keys actually supplied, each TRUE, must surface as attributes.
  res <- shinyTree:::jsonToAttr(list(make_state_node(list(selected = TRUE))))
  expect_true(attr(res$a, "stselected"))
  expect_null(attr(res$a, "stchecked"))

  res <- shinyTree:::jsonToAttr(list(make_state_node(list(checked = TRUE))))
  expect_true(attr(res$a, "stchecked"))
  expect_null(attr(res$a, "stselected"))

  res <- shinyTree:::jsonToAttr(list(make_state_node(list(disabled = TRUE, opened = TRUE))))
  expect_true(attr(res$a, "stdisabled"))
  expect_true(attr(res$a, "stopened"))
})

test_that("the registered input handler survives a treeToJSON-shaped payload", {
  handler <- shiny:::inputHandlers$get("shinyTree")
  val <- list(
    make_state_node(list(loaded = TRUE, opened = FALSE, selected = TRUE, disabled = FALSE),
                    text = "a", id = "1"),
    make_state_node(list(loaded = TRUE, opened = FALSE, selected = FALSE, disabled = FALSE),
                    text = "b", id = "2"),
    callbackCounter = 1
  )

  res <- handler(val, NULL, "tree")
  expect_equal(names(res), c("a", "b"))
  expect_equal(get_selected(res)[[1]][1], "a")
})

test_that("nested children with partial state are parsed and keep their ancestry", {
  parent <- make_state_node(list(loaded = TRUE, opened = TRUE, selected = FALSE,
                                 disabled = FALSE), text = "a", id = "1")
  parent$children <- list(
    make_state_node(list(loaded = TRUE, opened = FALSE, selected = TRUE,
                         disabled = FALSE), text = "leaf", id = "2")
  )

  tree <- shinyTree:::jsonToAttr(list(parent))
  sel <- get_selected(tree)
  expect_length(sel, 1)
  expect_equal(sel[[1]][1], "leaf")
  expect_equal(attr(sel[[1]], "ancestry"), "a")
})
