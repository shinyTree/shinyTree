library(shinytest2)

## This test has failed intermittently -- once in CI and once locally in about
## 36 full-suite runs -- without the cause being captured. If it fails, it
## prints a timeline from the test, the browser and the server so the next
## failure can be diagnosed. The browser log is kept by a script in the app.
print_rmarkdown_diagnostics <- function(app, trace, server_log) {
  cat("\n===== R Markdown test diagnostics =====\n")
  cat("--- test steps ---\n")
  cat(trace, sep = "\n")
  if (!is.null(app)) {
    read_js <- function(js) tryCatch(app$get_js(js), error = function(e) paste("unavailable:", conditionMessage(e)))
    cat("--- browser events ---\n")
    cat(tryCatch(jsonlite::fromJSON(app$get_js("JSON.stringify(window.__stlog || [])")),
                 error = function(e) paste("unavailable:", conditionMessage(e))), sep = "\n")
    cat("--- jsTree now: selected =", read_js("JSON.stringify($('#tree').jstree(true).get_selected())"),
        " nodes =", read_js("$('#tree li').length"), "\n")
    logs <- tryCatch(as.data.frame(app$get_logs()), error = function(e) NULL)
    if (!is.null(logs)) {
      cat("--- browser console and shiny warnings/errors ---\n")
      bad <- logs[logs$level %in% c("warning", "error"), c("location", "level", "message")]
      if (nrow(bad)) print(bad, right = FALSE) else cat("(none)\n")
    }
  }
  cat("--- server log ---\n")
  if (file.exists(server_log)) cat(readLines(server_log), sep = "\n") else cat("(empty)\n")
  cat("===== end diagnostics =====\n")
}

test_that("a tree renders in an R Markdown document with runtime: shiny (#18)", {
  ## R Markdown drops tags written into <head>, so when shinyTree put its
  ## scripts there jsTree never loaded and the tree stayed empty.
  skip_on_cran()
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("rmarkdown")
  skip_if_not(rmarkdown::pandoc_available(), "pandoc is not available")

  server_log <- tempfile("shinytree-rmarkdown-", fileext = ".log")
  withr::local_envvar(ST_DEBUG_LOG = server_log)
  t0 <- Sys.time()
  trace <- character()
  step <- function(...) {
    trace <<- c(trace, sprintf("%6.0fms %s", 1000 * as.numeric(Sys.time() - t0, units = "secs"),
                               paste0(...)))
  }
  app <- NULL
  passed <- FALSE
  withr::defer(if (!passed) print_rmarkdown_diagnostics(app, trace, server_log))

  step("starting AppDriver")
  app <- start_app(test_path("apps/test_rmarkdown"), "shinytree-rmarkdown")
  withr::defer(app$stop(), priority = "last")
  step("AppDriver started")

  app$wait_for_js("$('#tree li').length > 0", timeout = 20000)
  step("tree has ", app$get_js("$('#tree li').length"), " nodes")
  expect_equal(app$get_js("$('#tree li').length"), 2)

  ## The tree is a working input too. Wait for the selection to register in
  ## jsTree and then to reach the server: under covr in CI the input can lag
  ## behind wait_for_idle().
  step("clicking root1")
  app$click(selector = "#tree > ul > li:first-child > a.jstree-anchor")
  step("click returned; jsTree selected = ",
       app$get_js("JSON.stringify($('#tree').jstree(true).get_selected())"))
  app$wait_for_js("$('#tree').jstree(true).get_selected().length > 0")
  step("jsTree reports a selection")
  ## Shiny's own record of the input, so this waits for it to reach the server.
  app$wait_for_js("(function(){ var v = Shiny.shinyapp.$inputValues['tree:shinyTree'];
                     return !!v && JSON.stringify(v).indexOf('\"selected\":true') >= 0; })()",
                  timeout = 20000)
  app$wait_for_idle()
  selected <- get_selected(app$get_value(input = "tree"))
  step("selected = [", paste(unlist(selected), collapse = ","), "]")
  expect_length(selected, 1)
  expect_equal(selected[[1]][1], "root1")
  passed <- length(selected) == 1 && identical(selected[[1]][1], "root1")
})
