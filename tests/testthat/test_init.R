context("test_init")
library(shiny)
library(withr)

handler <- shiny:::inputHandlers$get("shinyTree")

make_node <- function(text, selected = FALSE, checked = FALSE, disabled = FALSE,
                       opened = FALSE, data = list(), id = NULL, children = list()) {
  node <- list(
    text = text,
    children = children,
    state = list(opened = opened, selected = selected, disabled = disabled, checked = checked),
    data = data
  )
  if (!is.null(id)) node$id <- id
  node
}

test_that("shinyTree input handler is registered", {
  expect_is(handler, "function")
})

test_that("jsonToAttr handles a top-level list of nodes (no 'text' key)", {
  val <- list(
    make_node("root1"),
    make_node("root2")
  )
  res <- shinyTree:::jsonToAttr(val)
  expect_is(res, "list")
  expect_setequal(names(res), c("root1", "root2"))
})

test_that("jsonToAttr recurses into a node's children", {
  node <- make_node("parent", children = list(make_node("child1"), make_node("child2")))
  res <- shinyTree:::jsonToAttr(node)
  expect_is(res, "list")
  expect_setequal(names(res), c("child1", "child2"))
})

test_that("jsonToAttr returns a leaf value of 0 with no extra attributes by default", {
  node <- make_node("leaf")
  res <- shinyTree:::jsonToAttr(list(node))
  expect_equal(res$leaf, structure(0, names = NULL))
  expect_null(attr(res$leaf, "stselected"))
  expect_null(attr(res$leaf, "stchecked"))
  expect_null(attr(res$leaf, "stdisabled"))
  expect_null(attr(res$leaf, "stopened"))
})

test_that("jsonToAttr sets st* attributes only when the corresponding state is TRUE", {
  node <- make_node("leaf", selected = TRUE, checked = TRUE, disabled = TRUE, opened = TRUE)
  res <- shinyTree:::jsonToAttr(list(node))
  expect_true(attr(res$leaf, "stselected"))
  expect_true(attr(res$leaf, "stchecked"))
  expect_true(attr(res$leaf, "stdisabled"))
  expect_true(attr(res$leaf, "stopened"))
})

test_that("jsonToAttr carries over data fields and id as attributes", {
  node <- make_node("leaf", data = list(foo = "bar", baz = 42), id = "node-id-1")
  res <- shinyTree:::jsonToAttr(list(node))
  expect_equal(attr(res$leaf, "foo"), "bar")
  expect_equal(attr(res$leaf, "baz"), 42)
  expect_equal(attr(res$leaf, "id"), "node-id-1")
})

test_that("jsonToAttr leaves id attribute unset when absent", {
  node <- make_node("leaf")
  res <- shinyTree:::jsonToAttr(list(node))
  expect_null(attr(res$leaf, "id"))
})

test_that("input handler strips callbackCounter and uses the list parser by default", {
  withr::local_options(shinyTree.defaultParser = "list")
  val <- list(make_node("root1", selected = TRUE), callbackCounter = 7)
  res <- handler(val, NULL, "tree")
  expect_is(res, "list")
  expect_equal(names(res), "root1")
  expect_true(attr(res$root1, "stselected"))
})

test_that("input handler uses the data.tree parser when requested", {
  skip_if_not_installed("data.tree")
  withr::local_options(shinyTree.defaultParser = "tree")
  val <- list(make_node("root1"), callbackCounter = 1)
  res <- suppressWarnings(handler(val, NULL, "tree"))
  expect_is(res, "Node")
})

test_that("input handler falls back to list parsing with a warning for an unknown parser", {
  withr::local_options(shinyTree.defaultParser = "bogus")
  val <- list(make_node("root1"), callbackCounter = 1)
  expect_warning(res <- handler(val, NULL, "tree"), "unknown parser")
  expect_is(res, "list")
  expect_equal(names(res), "root1")
})

test_that("input handler falls back to list parsing when data.tree cannot be loaded", {
  skip_if_not_installed("mockery")
  withr::local_options(shinyTree.defaultParser = "tree")
  mockery::stub(handler, "requireNamespace", FALSE)
  val <- list(make_node("root1"), callbackCounter = 1)
  expect_warning(res <- handler(val, NULL, "tree"), "data.tree")
  expect_is(res, "list")
  expect_equal(names(res), "root1")
})

test_that("initResourcePaths registers the resource path once and returns empty HTML", {
  global_env <- shinyTree:::.global
  withr::defer(global_env$loaded <- NULL)
  global_env$loaded <- NULL

  res1 <- shinyTree:::initResourcePaths()
  expect_identical(res1, shiny::HTML(""))
  expect_true(global_env$loaded)

  ## calling again should not error and should leave state untouched
  res2 <- shinyTree:::initResourcePaths()
  expect_identical(res2, shiny::HTML(""))
  expect_true(global_env$loaded)
})

test_that("supplementAttr returns NULL unchanged", {
  res <- shinyTree:::supplementAttr(NULL, make_node("leaf"))
  expect_null(res)
})
