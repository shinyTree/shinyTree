context("test_render_tree_async")
library(jsonlite)
library(promises)

make_fake_session <- function() {
  session <- shiny::MockShinySession$new()
  session$sent <- NULL
  session$sendInputMessage <- function(inputId, message) {
    session$sent <- list(inputId = inputId, message = message)
  }
  session
}

flush_promises <- function(times = 5, pause = 0.02) {
  for (i in seq_len(times)) {
    later::run_now()
    Sys.sleep(pause)
  }
}

test_that("renderTreeAsync returns a shiny render function", {
  rf <- renderTreeAsync({
    list(root1 = "")
  })
  expect_is(rf, "shiny.render.function")
  expect_is(rf, "function")
})

test_that("renderTreeAsync sends an updateTree message for a non-promise expression", {
  rf <- renderTreeAsync({
    list(root1 = "", root2 = list(leaf1 = ""))
  })

  session <- make_fake_session()
  shiny::withReactiveDomain(session, {
    rf(session, "tree")
  })
  flush_promises()

  expect_equal(session$sent$inputId, "tree")
  expect_equal(session$sent$message$type, "updateTree")
  expect_true(jsonlite::validate(session$sent$message$data))

  parsed <- jsonlite::fromJSON(session$sent$message$data, simplifyVector = FALSE)
  texts <- unlist(lapply(parsed, function(x) x$text))
  expect_setequal(texts, c("root1", "root2", "leaf1"))
})

test_that("renderTreeAsync resolves a promise-returning expression and sends the tree", {
  rf <- renderTreeAsync({
    promises::promise(function(resolve, reject) {
      resolve(list(root1 = "", root2 = ""))
    })
  })

  session <- make_fake_session()
  shiny::withReactiveDomain(session, {
    rf(session, "tree")
  })
  flush_promises()

  expect_equal(session$sent$message$type, "updateTree")
  expect_true(jsonlite::validate(session$sent$message$data))

  parsed <- jsonlite::fromJSON(session$sent$message$data, simplifyVector = FALSE)
  texts <- unlist(lapply(parsed, function(x) x$text))
  expect_setequal(texts, c("root1", "root2"))
})

test_that("renderTreeAsync does not send a message when the promise rejects", {
  rf <- renderTreeAsync({
    promises::promise(function(resolve, reject) {
      reject("boom")
    })
  })

  session <- make_fake_session()
  expect_no_condition <- tryCatch({
    shiny::withReactiveDomain(session, {
      rf(session, "tree")
    })
    flush_promises()
    TRUE
  }, error = function(e) FALSE)

  expect_true(expect_no_condition)
  expect_null(session$sent)
})

test_that("getAll reads existing bindings and returns NULL for missing ones", {
  env <- new.env()
  env$a <- 1
  env$b <- "two"

  vals <- shinyTree:::getAll(c("a", "b", "c"), env)
  expect_equal(vals$a, 1)
  expect_equal(vals$b, "two")
  expect_null(vals$c)
})

test_that("setAll assigns each named value into the target environment", {
  env <- new.env()
  shinyTree:::setAll(list(a = 10, b = "twenty", c = TRUE), env)

  expect_equal(env$a, 10)
  expect_equal(env$b, "twenty")
  expect_true(env$c)
})

test_that("tempVarsPromiseDomain wraps sync evaluation with temp var swapping", {
  env <- new.env()
  env$x <- "original"

  domain <- shinyTree:::tempVarsPromiseDomain(env, x = "swapped")
  result <- promises::with_promise_domain(domain, {
    inside <- env$x
    inside
  })

  expect_equal(result, "swapped")
  expect_equal(env$x, "original")
})

test_that("tempVarsPromiseDomain swaps temp vars while an onRejected handler runs", {
  env <- new.env()
  env$x <- "original"

  domain <- shinyTree:::tempVarsPromiseDomain(env, x = "swapped")

  result <- NULL
  p <- promises::promise(function(resolve, reject) reject("boom"))
  promises::with_promise_domain(domain, {
    promises::then(p, onRejected = function(e) {
      result <<- env$x
    })
  })
  flush_promises()

  expect_equal(result, "swapped")
  expect_equal(env$x, "original")
})
