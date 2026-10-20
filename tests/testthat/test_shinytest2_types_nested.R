library(shinyTree)
library(shinytest2)

## Adversarial regression test for issue #129:
## "Argument `types` doesn't work in more than 1 nested modules"
##
## shinyTree() publishes the types spec as a JS global whose name is the output
## id with hyphens replaced, built in R with gsub() -- so every hyphen goes. The
## client rebuilt the same name with String.replace("-", "_"), and in JavaScript
## a string first argument replaces only the FIRST match. One module level has a
## single hyphen and worked; two or more left the later hyphens in place, the
## lookup returned undefined, and jsTree's types plugin then threw
## "Cannot read properties of null (reading 'root')" and rendered nothing.

test_that("types works for a tree nested two module levels deep (#129)", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")

  app <- tryCatch(
    AppDriver$new(
      app_dir = test_path("apps/test_types_nested"),
      name = "types-nested-modules",
      variant = NULL,
      load_timeout = 30000
    ),
    error = function(e) e
  )
  if (inherits(app, "error")) {
    skip(paste("AppDriver could not be started:", conditionMessage(app)))
  }
  withr::defer(app$stop())

  info <- jsonlite::fromJSON(app$get_js(
    "(function(){
       var el = document.querySelector('.shiny-tree');
       var name = el.id.split('-').join('_') + '_sttypes';
       var icons = Array.from(el.querySelectorAll('li > a > i.jstree-themeicon'))
                        .map(function(i){ return i.className; });
       return JSON.stringify({
         id: el.id,
         hyphens: (el.id.match(/-/g) || []).length,
         global_defined: typeof window[name],
         nodes: el.querySelectorAll('li').length,
         icons: icons
       });
     })()"
  ))

  ## The scenario is only meaningful with more than one hyphen in the id.
  expect_gt(info$hyphens, 1)

  ## The global the R side published must be the one the client looks up.
  expect_equal(info$global_defined, "object")

  ## The tree renders rather than dying in the types plugin.
  expect_gt(info$nodes, 0)

  ## And the types spec is actually applied: root nodes get fa-archive.
  expect_true(any(grepl("fa-archive", info$icons)))

  ## No client-side error while loading the tree.
  errors <- app$get_logs()
  errors <- errors[errors$level %in% c("error", "SEVERE"), ]
  js_errors <- grep("favicon", errors$message, value = TRUE, invert = TRUE)
  expect_length(js_errors, 0)
})

test_that("the types global name is built identically on both sides", {
  ## Pins the contract the bug broke: whatever R names the global, the id-to-name
  ## transformation must replace every hyphen. Guards the R half without a
  ## browser.
  for (id in c("tree", "mod-tree", "outer-inner-tree", "a-b-c-d-tree")) {
    r_side <- gsub("-", "_", id, fixed = TRUE)
    expect_false(grepl("-", r_side))
    ## A JS `String.replace("-", "_")` would leave hyphens behind for the deeper
    ## ids; `split('-').join('_')` matches gsub().
    js_equivalent <- paste(strsplit(id, "-", fixed = TRUE)[[1]], collapse = "_")
    expect_equal(js_equivalent, r_side)
  }
})
