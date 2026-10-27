library(shinyTree)
library(shinytest2)

## One test per example, for examples 15 to 24, each checking what the
## example is there to show.

test_that("15-data: data stored on a node comes back with it", {
  app <- example_app("15-data")
  withr::defer(app$stop())
  ex_wait(app, "stAnchor('tree', 'root1') !== null")

  app$click(selector = node_anchor(app, "tree", "root1"))
  expect_match(wait_output(app, "selTxt", "Info for root1"), "Info for root1 : A")
  ex_js(app, "stTree('tree').open_all()")
  ex_wait(app, "stAnchor('tree', 'leaf2') !== null")
  app$click(selector = node_anchor(app, "tree", "leaf1"))
  expect_match(wait_output(app, "selTxt", "Info for leaf1"), "Info for leaf1 : B")
  ## A node without data reports none.
  app$click(selector = node_anchor(app, "tree", "leaf2"))
  out <- wait_output(app, "selTxt", "Info for leaf2")
  expect_false(grepl("Info for leaf1", out))
  expect_no_js_errors(app)
})

test_that("16-async: a tree built in another process arrives and works as an input", {
  skip_if_not_installed("promises")
  skip_if_not_installed("future")
  app <- example_app("16-async")
  withr::defer(app$stop())
  ## future's workers can be slow to start.
  ex_wait(app, "stTop('tree_async').join() === 'root1,root2'", timeout = 60000)
  expect_equal(ex_js(app, "stTop('tree')"), ex_js(app, "stTop('tree_async')"))
  expect_equal(ex_js(app, "stKids('tree_async', 'SubListA')"), list("leaf1", "leaf2", "leaf3"))

  ex_js(app, "stTree('tree_async').open_all()")
  ex_wait(app, "stAnchor('tree_async', 'leaf1') !== null")
  app$click(selector = node_anchor(app, "tree_async", "leaf1"))
  app$wait_for_idle()
  sel <- get_selected(app$get_value(input = "tree_async"))
  expect_length(sel, 1)
  expect_equal(sel[[1]][1], "leaf1")
  expect_equal(attr(sel[[1]], "stinfo"), "B")
  expect_no_js_errors(app)
})

test_that("17-contextmenu: nodes are created, renamed and deleted from the menu", {
  app <- example_app("17-contextmenu")
  withr::defer(app$stop())
  ex_wait(app, "stTop('tree').length === 3")
  ## sort = TRUE
  expect_equal(ex_js(app, "stTop('tree')"), list("root1", "root2", "root3"))

  ## Right-click shows the menu, whose items act on the node.
  app$run_js(sprintf("$('%s').trigger('contextmenu')", node_anchor(app, "tree", "root1")))
  ex_wait(app, "$('.vakata-context:visible').length > 0")
  items <- unlist(app$get_js("$('.vakata-context:visible > li > a').map(function(){ return $(this).text().trim(); }).get()"))
  expect_true(all(c("Create", "Rename", "Delete") %in% items))
  app$run_js("$('.vakata-context:visible > li > a').filter(function(){ return $(this).text().trim() === 'Delete'; }).trigger('click')")
  ex_wait(app, "stTop('tree').join() === 'root2,root3'")
  app$wait_for_idle()
  expect_equal(names(app$get_value(input = "tree")), c("root2", "root3"))

  ## A node created and renamed reaches the server under its new name.
  ex_js(app, "(function(t){ var id = t.create_node(stNode('tree', 'root2'), {text: 'temp'}); t.rename_node(id, 'added'); })(stTree('tree'))")
  app$wait_for_idle()
  expect_true("added" %in% names(app$get_value(input = "tree")$root2))

  ## The selection in each format.
  ex_js(app, "stTree('tree').open_all()")
  ex_wait(app, "stNodes('tree', 'leaf1').length === 2")
  ex_js(app, "stTree('tree').select_node(stNodes('tree', 'leaf1')[1])")
  out <- wait_output(app, "sel_slices", "leaf1")
  expect_match(out, "root3")
  expect_match(out, "SubListA")
  expect_match(wait_output(app, "sel_names", "leaf1"), "root3")
  expect_no_js_errors(app)
})

test_that("18-modules: trees inside Shiny modules, with and without types", {
  dir <- system.file("examples", "18-modules", package = "shinyTree")
  skip_if(dir == "", "example not installed")
  for (file in c("app.R", "app_types.R")) {
    ## AppDriver runs a directory's app.R, so each file gets a directory.
    app_dir <- file.path(withr::local_tempdir(), "app")
    dir.create(app_dir)
    file.copy(file.path(dir, file), file.path(app_dir, "app.R"))
    app <- start_app(app_dir, paste0("example-18-", sub("[.]R$", "", file)))
    ids <- if (file == "app.R") "tree_module-tree_with_type" else
      c("tree_module1-tree_with_type", "tree_module1-tree_without_type", "tree_module2-tree_with_type")
    for (id in ids) {
      ex_wait(app, sprintf("stTop('%s').length === 2", id))
      top <- unlist(ex_js(app, sprintf("stTop('%s')", id)))
      ## Two of the three roots, chosen at random, named for the module.
      prefix <- if (grepl("without", id)) "Module w/o Type_" else "Module w/ Type_"
      expect_true(all(startsWith(top, prefix)), info = id)
      expect_equal(ex_js(app, sprintf("stKids('%s', '%s')", id, top[1])), list("elem1", "elem2", "elem3"))
      ## The input is namespaced like the output.
      expect_false(is.null(app$get_value(input = id)), info = id)
    }
    ## Icons from sticon (app.R) or from the module's types (app_types.R).
    icon <- function(id, text) ex_js(app, sprintf(
      "$(stAnchor('%s', '%s')).find('i.jstree-themeicon').attr('class')", id, text))
    first <- unlist(ex_js(app, sprintf("stTop('%s')", ids[1])))[1]
    expect_match(icon(ids[1], first), "fa-archive")
    if (file == "app_types.R") {
      plain <- unlist(ex_js(app, "stTop('tree_module1-tree_without_type')"))[1]
      expect_false(grepl("fa-archive", icon("tree_module1-tree_without_type", plain)))
    }
    ## Go re-renders the module's tree.
    go <- if (file == "app.R") "tree_module-go" else "tree_module1-go"
    app$click(go)
    app$wait_for_idle()
    ex_wait(app, sprintf("stTop('%s').length === 2", ids[1]))
    expect_no_js_errors(app)
    app$stop()
  }
})

test_that("19-data.tree: a data.tree is shown, and read back as a data.tree or a list", {
  skip_if_not_installed("data.tree")
  skip_if_not_installed("shinyjs")
  app <- example_app("19-data.tree")
  withr::defer(app$stop())
  ex_wait(app, "stNode('tree', 'IT') !== null")
  ## li_attr and icon set on the data.tree's nodes
  expect_true(ex_js(app, "$(stAnchor('tree', 'IT')).parent().hasClass('myl')"))
  expect_equal(ex_js(app, "getComputedStyle(stAnchor('tree', 'IT')).color"), "rgb(255, 0, 0)")
  expect_match(ex_js(app, "$(stAnchor('tree', 'Accounting')).find('i.jstree-themeicon').attr('class')"), "file")
  expect_match(wait_output(app, "json", "Accounting"), '"text": "Accounting"')

  out <- wait_output(app, "output_tree", "Accounting")
  expect_match(out, "IT")
  app$set_inputs(parser = "list")
  out <- wait_output(app, "output_tree", "List of")
  expect_match(out, "Accounting")
  expect_no_js_errors(app)
})

test_that("20-api: jsTree's own API, called from R through shinyjs", {
  skip_if_not_installed("shinyjs")
  app <- example_app("20-api")
  withr::defer(app$stop())
  ex_wait(app, "stNode('tree', 'SubListB') !== null")
  ex_js(app, "stTree('tree').open_all()")
  app$click(selector = node_checkbox(app, "tree", "root1"))
  ex_wait(app, "stTree('tree').get_selected().length > 1 && $('#tree li.jstree-open').length > 0")

  app$click("reset")
  ex_wait(app, "stTree('tree').get_selected().length === 0 && $('#tree li.jstree-open').length === 0")
  app$wait_for_idle()
  expect_length(get_selected(app$get_value(input = "tree")), 0)
  expect_no_js_errors(app)
})

test_that("21-options: shinyTree.refresh reports a refresh started in the browser", {
  app <- example_app("21-options")
  withr::defer(app$stop())
  count <- function(id) as.integer(sub(".*updates: ([0-9]+).*", "\\1", app$get_text(paste0("#", id, "_count"))))
  for (id in c("reported", "silent")) {
    ex_wait(app, sprintf("stAnchor('%s', 'a1') !== null", id))
    app$click(selector = node_anchor(app, id, "a1"))
    wait_output(app, paste0(id, "_count"), "selected: a1")
  }
  app$wait_for_idle()
  silent_before <- count("silent")
  reported_before <- count("reported")

  ## refresh(true, true) from JavaScript clears the selection in both trees,
  ## but only the tree with the option on reports it.
  app$click("js_refresh")
  ex_wait(app, "stTree('reported').get_selected().length === 0 && stTree('silent').get_selected().length === 0")
  wait_output(app, "reported_count", "selected: $")
  app$wait_for_idle(duration = 1000)
  expect_gt(count("reported"), reported_before)
  expect_equal(count("silent"), silent_before)
  expect_match(app$get_text("#silent_count"), "selected: a1")

  ## A tree sent by the server is reported by both.
  app$click("server_update")
  wait_output(app, "silent_count", "selected: $")
  expect_gt(count("silent"), silent_before)
  expect_no_js_errors(app)
})

test_that("22-dfToTree: a data frame becomes a tree, and the tree a data frame", {
  app <- example_app("22-dfToTree")
  withr::defer(app$stop())
  ex_wait(app, "stTop('tree').length === 2")
  expect_equal(ex_js(app, "stTop('tree')"), list("Male", "Female"))
  expect_equal(ex_js(app, "stKids('tree', 'Male')"), list("1st", "2nd", "3rd", "Crew"))
  rows <- function() app$get_js("document.querySelectorAll('#df table tbody tr').length")
  app$wait_for_js("document.querySelectorAll('#df table tbody tr').length === 16")
  header <- unlist(app$get_js("Array.from(document.querySelectorAll('#df table thead th')).map(function(x){ return x.textContent.trim(); })"))
  expect_equal(header, c("Sex", "Class", "Survived"))

  ## The table, from treeToDf(input$tree), has a row for each combination in
  ## the data frame the tree was built from.
  table <- jsonlite::fromJSON(app$get_js(
    "JSON.stringify(Array.from(document.querySelectorAll('#df table tbody tr')).map(function(r){ return Array.from(r.cells).map(function(c){ return c.textContent.trim(); }); }))"))
  titanic <- unique(data.frame(Titanic)[, c("Sex", "Class", "Survived")])
  expect_setequal(apply(table, 1, paste, collapse = "/"),
                  apply(as.matrix(titanic), 1, paste, collapse = "/"))
  expect_equal(rows(), 16)
  expect_no_js_errors(app)
})

test_that("23-file-icons: set_node_attrs() gives folders and files their icons", {
  skip_if_not_installed("tidyr")
  app <- example_app("23-file-icons")
  withr::defer(app$stop())
  ex_wait(app, "stNode('tree', 'examples') !== null")
  icon <- function(text) ex_js(app, sprintf("$(stAnchor('tree', '%s')).find('i.jstree-themeicon').attr('class')", text))
  type <- function(text) ex_js(app, sprintf("stTree('tree').get_node(stNode('tree', '%s')).type", text))
  expect_equal(type("examples"), "directory")
  expect_match(icon("examples"), "glyphicon-folder-open")
  expect_true("23-file-icons" %in% unlist(ex_js(app, "stKids('tree', 'examples')")))
  expect_equal(type("DESCRIPTION"), "file")
  expect_match(icon("DESCRIPTION"), "glyphicon-file")
  ## Nested as deep as the files are: examples / 23-file-icons / server.R
  expect_equal(sort(unlist(ex_js(app, "stKids('tree', '23-file-icons')"))), c("server.R", "ui.R"))
  expect_no_js_errors(app)
})

test_that("24-checkbox-options: checking and selecting are kept apart", {
  app <- example_app("24-checkbox-options")
  withr::defer(app$stop())
  ex_wait(app, "stNode('tree', 'SubListB') !== null")
  app$wait_for_idle()
  checked <- function() unlist(get_checked(app$get_value(input = "tree")))
  selected <- function() unlist(get_selected(app$get_value(input = "tree")))
  ## stchecked on SubListB, and three_state = FALSE: its children are not checked.
  expect_equal(checked(), "SubListB")
  expect_null(selected())

  ## whole_node = FALSE: clicking the label selects without checking.
  app$click(selector = node_anchor(app, "tree", "root1"))
  app$wait_for_idle()
  expect_equal(selected(), "root1")
  expect_equal(checked(), "SubListB")

  ## Ticking a box checks only that node, and leaves the selection alone.
  ex_js(app, "stTree('tree').open_all()")
  ex_wait(app, "stAnchor('tree', 'leaf1') !== null")
  app$click(selector = node_checkbox(app, "tree", "leaf1"))
  app$wait_for_idle()
  expect_setequal(checked(), c("leaf1", "SubListB"))
  expect_equal(selected(), "root1")
  expect_match(app$get_text("#checked"), "leaf1")
  expect_no_js_errors(app)
})
