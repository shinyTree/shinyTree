library(shinyTree)
library(shinytest2)

## One test per example, for examples 08 to 14, each checking what the
## example is there to show.

test_that("08-class: types and stcssclass style nodes", {
  app <- example_app("08-class")
  withr::defer(app$stop())
  ex_wait(app, "stAnchor('tree', 'Tidy desk') !== null")
  colour <- function(text) ex_js(app, sprintf("getComputedStyle(stAnchor('tree', '%s')).color", text))
  style <- function(text) ex_js(app, sprintf(
    "(function(a){ var s = getComputedStyle(a); return [s.textDecorationLine, s.fontWeight]; })(stAnchor('tree', '%s'))", text))

  ## sttype: each type's a_attr style
  expect_equal(colour("root1"), "rgb(255, 0, 0)")
  expect_equal(colour("root2"), "rgb(0, 0, 255)")
  ## stcssclass: any number of classes, styled by the app's CSS
  expect_true(ex_js(app, "$(stAnchor('tree', 'Write report')).parent().is('.done.important')"))
  expect_equal(unlist(style("Write report")), c("line-through", "700"))
  expect_equal(unlist(style("Book travel")), c("line-through", "400"))
  expect_equal(unlist(style("Call client")), c("none", "700"))
  expect_equal(unlist(style("Tidy desk")), c("none", "400"))
  expect_no_js_errors(app)
})

test_that("09-themes: each tree shows its theme, with or without icons and dots", {
  app <- example_app("09-themes")
  withr::defer(app$stop())
  for (id in c("tree", "tree2", "tree3", "tree4")) {
    ex_wait(app, sprintf("stTop('%s').length === 2", id))
  }
  has <- function(id, class) ex_js(app, sprintf("$('#%s').find('.jstree-container-ul').addBack().hasClass('%s')", id, class))
  expect_true(has("tree", "jstree-default"))
  expect_true(has("tree2", "jstree-default-dark"))
  expect_true(has("tree3", "jstree-proton"))
  expect_true(has("tree4", "jstree-proton"))
  ## themeIcons = FALSE, themeDots = FALSE on the fourth only
  expect_true(has("tree4", "jstree-no-icons"))
  expect_true(has("tree4", "jstree-no-dots"))
  expect_false(has("tree3", "jstree-no-icons"))
  expect_false(has("tree3", "jstree-no-dots"))
  ## The dark theme really is dark.
  expect_false(identical(
    ex_js(app, "getComputedStyle(document.querySelector('#tree2 .jstree-container-ul').closest('.jstree')).backgroundColor"),
    ex_js(app, "getComputedStyle(document.querySelector('#tree .jstree-container-ul').closest('.jstree')).backgroundColor")))
  expect_no_js_errors(app)
})

test_that("10-node-ids: stid and stclass come back with get_selected(format = 'classid')", {
  app <- example_app("10-node-ids")
  withr::defer(app$stop())
  ex_wait(app, "stAnchor('tree', 'Study B-A') !== null")

  app$click(selector = node_checkbox(app, "tree", "Study B-A"))
  app$wait_for_idle()
  sel <- get_selected(app$get_value(input = "tree"), format = "classid")
  expect_length(sel, 1)
  expect_equal(sel[[1]][1], "Study B-A")
  expect_equal(attr(sel[[1]], "stid"), 3)
  expect_equal(attr(sel[[1]], "stclass"), "study")
  out <- wait_output(app, "selTxt", "Study B-A")
  expect_match(out, "study")

  ## Ticking a project cascades to its studies, each with its own id.
  app$click(selector = node_checkbox(app, "tree", "Project A"))
  app$wait_for_idle()
  sel <- get_selected(app$get_value(input = "tree"), format = "classid")
  ids <- setNames(vapply(sel, function(x) as.numeric(attr(x, "stid")), 0), vapply(sel, `[`, "", 1))
  expect_equal(ids[c("Study A-A", "Study A-B", "Project A", "Study B-A")],
               c(`Study A-A` = 1, `Study A-B` = 2, `Project A` = 1, `Study B-A` = 3))
  expect_equal(attr(sel[[which(names(ids) == "Project A")]], "stclass"), "project")
  expect_no_js_errors(app)
})

test_that("11-tree-update: updateTree() fills an empty tree; expand and collapse all", {
  app <- example_app("11-tree-update")
  withr::defer(app$stop())
  ## renderEmptyTree() until a tree is chosen.
  app$wait_for_idle()
  expect_equal(app$get_js("document.querySelectorAll('#tree li').length"), 0)

  app$set_inputs(idTree = "A")
  ex_wait(app, "stAnchor('tree', 'Child 1') !== null")
  expect_true(ex_js(app, "stTree('tree').is_open(stNode('tree', 'Root node with options'))"))
  wait_output(app, "idSelected", "Root node with options")

  app$set_inputs(idTree = "A_closed")
  ex_wait(app, "!stTree('tree').is_open(stNode('tree', 'Root node with options'))")
  app$click("expand")
  ex_wait(app, "stTree('tree').is_open(stNode('tree', 'Root node with options'))")
  app$click("collapse")
  ex_wait(app, "!stTree('tree').is_open(stNode('tree', 'Root node with options'))")

  ## JSON with li_attr, and then an R list.
  app$set_inputs(idTree = "B")
  ex_wait(app, "stTop('tree').join() === 'Simplerootnode,Rootnode2'")
  expect_true(ex_js(app, "$(stAnchor('tree', 'Simplerootnode')).parent().hasClass('project')"))
  app$set_inputs(idTree = "List")
  ex_wait(app, "stTop('tree').join() === 'a,b'")
  expect_equal(ex_js(app, "stKids('tree', 'a')"), list("a1", "a2"))
  expect_no_js_errors(app)
})

test_that("12-types: types give icons and limit where nodes can go", {
  app <- example_app("12-types")
  withr::defer(app$stop())
  ex_wait(app, "stAnchor('tree', 'leaf1') !== null")
  icon <- function(text) ex_js(app, sprintf("$(stAnchor('tree', '%s')).find('i.jstree-themeicon').attr('class')", text))
  check <- function() {
    for (node in c("root1", "root2", "SubListA", "SubListB")) expect_match(icon(node), "fa-signal", info = node)
    for (node in c("leaf1", "leafA")) expect_match(icon(node), "glyphicon-file", info = node)
    expect_true(ex_js(app, "stTree('tree').is_selected(stNode('tree', 'root1'))"))
  }
  check()

  ## A file allows no children, so a move into one is refused.
  ex_js(app, "stTree('tree').move_node(stNode('tree', 'leaf1'), stNode('tree', 'leafA'))")
  expect_equal(ex_js(app, "stKids('tree', 'SubListA')"), list("leaf1", "leaf2", "leaf3"))
  ## The root allows at most two top-level nodes.
  ex_js(app, "stTree('tree').move_node(stNode('tree', 'SubListB'), '#')")
  expect_equal(ex_js(app, "stTop('tree')"), list("root1", "root2"))

  ## Sending the same tree with updateTree() gives the same result.
  app$click("updateTree")
  app$wait_for_idle()
  ex_wait(app, "stAnchor('tree', 'leaf1') !== null")
  check()
  expect_no_js_errors(app)
})

test_that("13-icons: sticon and types set icons, in every form", {
  app <- example_app("13-icons")
  withr::defer(app$stop())
  ex_wait(app, "stAnchor('tree', 'leafC') !== null")
  icon <- function(text) ex_js(app, sprintf("$(stAnchor('tree', '%s')).find('i.jstree-themeicon').attr('class')", text))
  ## sticon wins over the node's type
  expect_match(icon("leaf1"), "fa-signal")
  ## the type's icon otherwise
  expect_match(icon("leaf2"), "fa-file")
  ## a glyphicon, an image, and a bare Font Awesome name
  expect_match(icon("leafA"), "glyphicon-leaf")
  expect_match(ex_js(app, "$(stAnchor('tree', 'leafB')).find('i.jstree-themeicon').css('background-image')"),
               "shinyTree/icon.png")
  expect_match(icon("leafC"), "fa fa-map-signs")
  ## The image is served.
  expect_equal(app$get_js("fetch('shinyTree/icon.png').then(function(r){ return r.status; })"), 200)
  expect_no_js_errors(app)
})

test_that("14-files: the file system shown as a tree of folders and files", {
  app <- example_app("14-files")
  withr::defer(app$stop())
  ex_wait(app, "stNode('tree', '01-simple') !== null")
  top <- unlist(ex_js(app, "stTop('tree')"))
  expect_true(all(c("01-simple", "06-search", "14-files") %in% top))
  expect_match(ex_js(app, "$(stAnchor('tree', '01-simple')).find('i.jstree-themeicon').attr('class')"),
               "glyphicon-folder-open")
  ## Every file in a folder, with the file icon.
  expect_equal(sort(unlist(ex_js(app, "stKids('tree', '01-simple')"))), c("server.R", "ui.R"))
  expect_true("texasCities.Rds" %in% unlist(ex_js(app, "stKids('tree', '06-search')")))
  ex_js(app, "stTree('tree').open_node(stNode('tree', '01-simple'))")
  ex_wait(app, "stAnchor('tree', 'server.R') !== null")
  expect_match(ex_js(app, "$(stAnchor('tree', 'server.R')).find('i.jstree-themeicon').attr('class')"),
               "glyphicon-file")
  expect_no_js_errors(app)
})
