library(shinyTree)
library(shinytest2)

## Every example app starts, shows a tree and raises no JavaScript error.
## Several examples had stopped working without anyone noticing -- one used a
## removed future plan, another an old shinyjs signature -- because nothing ran
## them. This is the cheap check that catches that; examples with behaviour
## worth more have their own test files.
##
## The installed copy is used, as for the other example tests, since R CMD
## check runs the tests against the installed package.

examples <- list(
  "01-simple" = "tree",
  "02-attributes" = "tree",
  "03-checkbox" = "tree",
  "04-selected" = "tree123",
  "05-structure" = "tree",
  "06-search" = "tree",
  "07-drag-and-drop" = "tree",
  "08-class" = "tree",
  "09-themes" = "tree",
  "10-node-ids" = "tree",
  "12-types" = "tree",
  "13-icons" = "tree",
  "14-files" = "tree",
  "15-data" = "tree",
  "16-async" = c("tree", "tree_async"),
  "17-contextmenu" = "tree",
  "19-data.tree" = "tree",
  "20-api" = "tree",
  "21-options" = c("reported", "silent"),
  "22-dfToTree" = "tree",
  "23-file-icons" = "tree",
  "24-checkbox-options" = "tree",
  "26-state" = "saved",
  "28-ontology" = "tree"
)
## The packages each example needs beyond shiny and shinyTree.
needs <- list("16-async" = c("promises", "future"), "19-data.tree" = c("data.tree", "shinyjs"),
              "20-api" = "shinyjs", "23-file-icons" = "tidyr", "28-ontology" = "xml2")

for (name in names(examples)) {
  local({
    name <- name
    trees <- examples[[name]]
    test_that(sprintf("the %s example starts and shows its tree", name), {
      for (pkg in needs[[name]]) skip_if_not_installed(pkg)
      dir <- system.file("examples", name, package = "shinyTree")
      skip_if(dir == "", paste(name, "example not installed"))

      app <- start_app(dir, paste0("example-", name))
      withr::defer(app$stop())
      app$run_js("window.__stErrors = []; window.addEventListener('error', function(e) { window.__stErrors.push(e.message); });")
      for (tree in trees) {
        app$wait_for_js(sprintf("document.querySelectorAll('[id=\"%s\"] li').length > 0", tree),
                        timeout = 20000)
      }
      app$wait_for_idle()
      expect_equal(app$get_js("window.__stErrors.join('; ')"), "", info = name)
      ## Shiny logs errors from input and output bindings rather than throwing.
      logs <- app$get_logs()
      errors <- logs[logs$location == "chromote" & logs$level == "error", "message", drop = TRUE]
      errors <- errors[!grepl("Failed to load resource|favicon", errors)]
      expect_equal(errors, character(0), info = name)
    })
  })
}

test_that("the 11-tree-update example fills its empty tree with updateTree()", {
  dir <- system.file("examples", "11-tree-update", package = "shinyTree")
  skip_if(dir == "", "example not installed")
  app <- start_app(dir, "example-11-tree-update")
  withr::defer(app$stop())
  ## renderEmptyTree() until a tree is chosen, then filled by updateTree().
  expect_equal(app$get_js("document.querySelectorAll('#tree li').length"), 0)
  app$set_inputs(idTree = "B")
  app$wait_for_js("$('#tree a.jstree-anchor').first().text() === 'Simplerootnode'", timeout = 20000)
  expect_equal(app$get_js("$('#tree > ul > li').length"), 2)
})

test_that("the 18-modules examples start and show their trees", {
  dir <- system.file("examples", "18-modules", package = "shinyTree")
  skip_if(dir == "", "example not installed")
  for (file in c("app.R", "app_types.R")) {
    app <- start_app(file.path(dir, file), paste0("example-18-", sub("[.]R$", "", file)))
    app$wait_for_js("document.querySelectorAll('.shiny-tree li').length > 0", timeout = 20000)
    expect_gt(app$get_js("document.querySelectorAll('.shiny-tree li').length"), 0)
    app$stop()
  }
})
