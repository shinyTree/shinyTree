library(shinyTree)
library(shinytest2)

## Adversarial regression test for issue #113:
## "The search feature does not work as expected when proton theme is selected"
##
## jsTree's search plugin marks non-matching nodes with the .jstree-hidden
## class; the theme stylesheet is what turns that into display:none. The
## default and default-dark themes ship
##
##   .jstree-hidden,.jstree-node.jstree-hidden{display:none}
##
## but proton's stylesheet omits it, so searching a proton tree marked the right
## nodes and the browser painted all of them anyway. shinyTree now declares the
## rule in its own stylesheet, loaded after the theme.
##
## Two things this test has to get right, both learned the hard way:
##
##  * It must assert on what is *painted* (offsetParent), not which classes are
##    applied. The class was always correct, so a class-based test passes
##    against the bug.
##  * Each theme needs its own app. A first version rendered all three themes on
##    one page, and the default theme's stylesheet then supplied the missing rule
##    for the proton tree -- the test passed against the unfixed build because
##    the page accidentally cured the bug.

search_and_measure <- function(app, term) {
  app$run_js(sprintf(
    "document.getElementById('tree-search-input').value = '%s';
     $('#tree-search-input').trigger('keyup');", term
  ))
  ## The search box debounces (searchtime defaults to 250ms).
  Sys.sleep(1)
  app$wait_for_idle()

  jsonlite::fromJSON(app$get_js(
    "(function(){
       var lis = Array.from(document.querySelectorAll('#tree li'));
       var painted = lis.filter(function(l){ return l.offsetParent !== null; });
       var hidden = lis.filter(function(l){ return l.classList.contains('jstree-hidden'); });
       return JSON.stringify({
         total: lis.length,
         marked_hidden: hidden.length,
         painted: painted.length,
         computed: hidden.length ? getComputedStyle(hidden[0]).display : 'na',
         painted_texts: painted.map(function(l){
           var a = l.querySelector('a'); return a ? a.textContent.trim() : '';
         })
       });
     })()"
  ))
}

expect_search_filters <- function(app_dir, name) {
  app <- tryCatch(
    AppDriver$new(app_dir = test_path(app_dir), name = name, variant = NULL,
                  load_timeout = 30000),
    error = function(e) e
  )
  if (inherits(app, "error")) {
    skip(paste("AppDriver could not be started:", conditionMessage(app)))
  }
  withr::defer(app$stop(), envir = parent.frame())

  res <- search_and_measure(app, "3rd")

  ## jsTree marked some nodes as filtered out ...
  expect_gt(res$marked_hidden, 0)
  ## ... and the browser must actually stop painting them.
  expect_equal(res$computed, "none")
  expect_lt(res$painted, res$total)
  ## Nothing the search rejected may remain on screen.
  expect_false("1st" %in% res$painted_texts)
  expect_false("2nd" %in% res$painted_texts)
  expect_false("Crew" %in% res$painted_texts)
  ## The matches are still visible.
  expect_true("3rd" %in% res$painted_texts)
}

test_that("search filtering is painted with the default theme", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")
  expect_search_filters("apps/test_search_default", "search-theme-default")
})

test_that("search filtering is painted with the default-dark theme", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")
  expect_search_filters("apps/test_search_default_dark", "search-theme-default-dark")
})

test_that("search filtering is painted with the proton theme (#113)", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")
  expect_search_filters("apps/test_search_proton", "search-theme-proton")
})

test_that("shinyTree ships the rule that hides filtered nodes", {
  ## Static guard: if a theme is added or re-vendored without the rule, this
  ## stylesheet must still cover it.
  own_css <- system.file("www/shinyTree.css", package = "shinyTree")
  expect_true(nzchar(own_css))
  expect_match(paste(readLines(own_css, warn = FALSE), collapse = " "),
               "jstree-hidden")
})
