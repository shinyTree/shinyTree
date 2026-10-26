## Start a shinytest2 AppDriver for a test app.
##
## The test is skipped only when there is no Chrome to drive. Any other failure
## to start -- an app that errors, or never settles -- fails the test: every
## browser test used to catch all errors and skip, so an app broken by a
## package change turned the suite into skips and CI stayed green. A start that
## times out is tried once more, since a loaded machine can be slow to launch
## Chrome; one that times out twice fails.
start_app <- function(app_dir, name, ..., load_timeout = 60000) {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("shinytest2")
  ## chromote comes with shinytest2; asked through getExportedValue() so it
  ## need not be declared as a test dependency of its own.
  find_chrome <- getExportedValue("chromote", "find_chrome")
  chrome <- tryCatch(find_chrome(), error = function(e) NULL)
  if (is.null(chrome) || !nzchar(chrome)) {
    testthat::skip("No Chrome or Chromium found for shinytest2")
  }
  start <- function() {
    shinytest2::AppDriver$new(app_dir = app_dir, name = name, variant = NULL,
                              load_timeout = load_timeout, ...)
  }
  app <- tryCatch(start(), error = function(e) e)
  if (inherits(app, "error") && grepl("did not become stable|timed? ?out", conditionMessage(app),
                                      ignore.case = TRUE)) {
    app <- start()
  } else if (inherits(app, "error")) {
    stop(app)
  }
  app
}
