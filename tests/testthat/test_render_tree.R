context("test_render_tree")
library(jsonlite)

test_that("renderTree returns a render function", {
  rf <- renderTree({
    list(root1 = "")
  })
  expect_is(rf, "function")
})

test_that("renderTree sends an updateTree message with valid JSON through the session", {
  rf <- renderTree({
    list(
      root1 = "",
      root2 = list(
        SubListA = list(leaf1 = "", leaf2 = ""),
        SubListB = list(leafA = "")
      )
    )
  })

  sent <- NULL
  fake_session <- list(sendInputMessage = function(inputId, message) {
    sent <<- list(inputId = inputId, message = message)
  })

  res <- rf(fake_session, "tree")
  expect_null(res)
  expect_equal(sent$inputId, "tree")
  expect_equal(sent$message$type, "updateTree")
  expect_true(jsonlite::validate(sent$message$data))

  parsed <- jsonlite::fromJSON(sent$message$data, simplifyVector = FALSE)
  texts <- unlist(lapply(parsed, function(x) x$text))
  expect_setequal(texts, c("root1", "root2", "SubListA", "leaf1", "leaf2", "SubListB", "leafA"))
})

test_that("renderTree re-evaluates expr on every invocation", {
  counter <- 0
  rf <- renderTree({
    counter <<- counter + 1
    list(root = as.character(counter))
  })

  sent <- list()
  fake_session <- list(sendInputMessage = function(inputId, message) {
    sent[[length(sent) + 1]] <<- message
  })

  rf(fake_session, "tree")
  rf(fake_session, "tree")

  expect_equal(counter, 2)
  expect_length(sent, 2)
})

test_that("renderEmptyTree returns a no-op render function", {
  rf <- renderEmptyTree()
  expect_is(rf, "function")

  sent <- NULL
  fake_session <- list(sendInputMessage = function(inputId, message) {
    sent <<- message
  })

  res <- rf(fake_session, "tree")
  expect_null(res)
  expect_null(sent)
})
