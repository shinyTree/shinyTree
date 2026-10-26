library(shinytest2)

start_shared_app <- function(dir, name) {
  ## shinytest2 runs its own check for shared ids on the server side, which
  ## the second app trips on purpose.
  app <- start_app(test_path(dir), name, check_names = FALSE)
  app$wait_for_js("$('#tree li').length > 0")
  app$wait_for_idle()
  app
}

## Every shinyTree is an input and an output under the same id. Shiny reports
## each shared id as a likely mistake, in the console and, in dev mode, in an
## on-page error panel, so every app with a tree showed a false alarm (#110).
## Both come from the same client message, so the console log is checked here;
## Shiny disables dev mode under testthat, so the panel cannot be.


shared_id_warnings <- function(app) {
  logs <- as.data.frame(app$get_logs())
  logs$message[grepl("Shared input/output ID", logs$message, fixed = TRUE)]
}

test_that("a tree's own shared input/output id is not reported", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")

  app <- start_shared_app("apps/test_shared_id", "shinytree-shared-id")
  withr::defer(app$stop())

  expect_length(shared_id_warnings(app), 0)
})

test_that("a genuine shared id next to a tree is still reported", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")

  app <- start_shared_app("apps/test_shared_id_real", "shinytree-shared-id-real")
  withr::defer(app$stop())

  warnings <- shared_id_warnings(app)
  expect_length(warnings, 1)
  expect_match(warnings, "\"dup\": 1 input and 1 output", fixed = TRUE)
})
