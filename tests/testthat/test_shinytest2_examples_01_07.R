library(shinyTree)
library(shinytest2)

## One test per example, for examples 01 to 07, each checking what the
## example is there to show.

test_that("01-simple: a nested list becomes a tree, one node selected at a time", {
  app <- example_app("01-simple")
  withr::defer(app$stop())
  ex_wait(app, "stTop('tree').length === 3")

  expect_equal(ex_js(app, "stTop('tree')"), list("root1", "root2", "root3"))
  expect_equal(ex_js(app, "stKids('tree', 'root2')"), list("SubListA", "SubListB"))
  expect_equal(ex_js(app, "stKids('tree', 'SubListB')"), list("leafA", "leafB"))
  expect_true(ex_js(app, "$('#tree').hasClass('jstree-striped') || $('#tree .jstree-striped').length > 0"))

  ## multiple = FALSE: a ctrl-click on a second node replaces the selection.
  app$click(selector = node_anchor(app, "tree", "root1"))
  ex_js(app, "$(stAnchor('tree', 'root3')).trigger($.Event('click', {ctrlKey: true}))")
  ex_wait(app, "stTree('tree').get_selected().length === 1")
  expect_equal(ex_js(app, "stLabels('tree', stTree('tree').get_selected())"), list("root3"))
  expect_no_js_errors(app)
})

test_that("02-attributes: st* attributes select, open, disable, give icons and tooltips", {
  app <- example_app("02-attributes")
  withr::defer(app$stop())
  ex_wait(app, "stAnchor('tree', 'SubListB') !== null")

  ## root1: stselected, sticon = "signal" (expanded to "fa fa-signal"), sttooltip
  expect_true(ex_js(app, "$(stAnchor('tree', 'root1')).hasClass('jstree-clicked')"))
  expect_true(ex_js(app, "$(stAnchor('tree', 'root1')).find('i.jstree-themeicon').hasClass('fa-signal')"))
  expect_equal(ex_js(app, "stAnchor('tree', 'root1').title"), "Hover text set with sttooltip")
  ## root2: stopened
  expect_true(ex_js(app, "stTree('tree').is_open(stNode('tree', 'root2'))"))
  ## SubListB: stdisabled -- and clicking it does not select it.
  expect_true(ex_js(app, "stTree('tree').is_disabled(stNode('tree', 'SubListB'))"))
  app$click(selector = node_anchor(app, "tree", "SubListB"))
  app$wait_for_idle()
  expect_false(ex_js(app, "stTree('tree').is_selected(stNode('tree', 'SubListB'))"))
  expect_no_js_errors(app)
})

test_that("03-checkbox: checkboxes select several nodes, cascading to children", {
  app <- example_app("03-checkbox")
  withr::defer(app$stop())
  ex_wait(app, "stNode('tree', 'SubListB') !== null")

  expect_gt(ex_js(app, "$('#tree .jstree-checkbox').length"), 0)
  ## SubListB is rendered stselected, which three_state passes to its children.
  expect_true(ex_js(app, "stTree('tree').is_checked(stNode('tree', 'leafA'))"))
  expect_true(ex_js(app, "stTree('tree').is_checked(stNode('tree', 'leafB'))"))

  ## Ticking another box adds to the selection rather than replacing it.
  app$click(selector = node_checkbox(app, "tree", "root1"))
  ex_wait(app, "stTree('tree').is_checked(stNode('tree', 'root1'))")
  expect_true(ex_js(app, "stTree('tree').is_checked(stNode('tree', 'leafA'))"))
  ## ... and it reaches the server.
  app$wait_for_idle()
  sel <- unlist(get_selected(app$get_value(input = "tree")))
  expect_true(all(c("root1", "leafA", "leafB") %in% sel))
  expect_no_js_errors(app)
})

test_that("05-structure: input$tree mirrors the tree, attributes included", {
  app <- example_app("05-structure")
  withr::defer(app$stop())
  out <- wait_output(app, "str", "root1")
  expect_match(out, "List of 2")
  expect_false(grepl("stselected", out))

  app$click(selector = node_anchor(app, "tree", "root1"))
  out <- wait_output(app, "str", "stselected")
  expect_match(out, 'attr\\(\\*, "stselected"\\)= logi TRUE')

  ex_js(app, "stTree('tree').open_node(stNode('tree', 'root2'))")
  out <- wait_output(app, "str", "stopened")
  expect_match(out, 'attr\\(\\*, "stopened"\\)= logi TRUE')
  expect_no_js_errors(app)
})

test_that("06-search: the search box highlights matches and opens their branches", {
  app <- example_app("06-search")
  withr::defer(app$stop())
  ex_wait(app, "stTop('tree').length === 9")
  expect_false(ex_js(app, "stTree('tree').is_open(stNode('tree', 'Northwest'))"))

  app$set_inputs(`tree-search-input` = "Mata", allow_no_input_binding_ = TRUE)
  app$run_js("$('#tree-search-input').trigger('input')")
  ## searchtime = 1000
  ex_wait(app, "$('#tree a.jstree-search').length === 3")
  found <- ex_js(app, "$('#tree a.jstree-search').map(function(){ return $(this).text(); }).get().sort()")
  expect_equal(unlist(found), c("Matador", "Matagorda", "Matagorda"))
  expect_true(ex_js(app, "stTree('tree').is_open(stNode('tree', 'Northwest'))"))
  expect_true(ex_js(app, "stTree('tree').is_open(stNode('tree', 'Motley'))"))
  ## Nodes that do not match are not marked.
  expect_false(ex_js(app, "$(stAnchor('tree', 'Motley')).hasClass('jstree-search')"))

  app$run_js("$('#tree-search-input').val('').trigger('input')")
  ex_wait(app, "$('#tree a.jstree-search').length === 0")
  expect_no_js_errors(app)
})

test_that("07-drag-and-drop: a node moved in the browser moves in input$tree", {
  app <- example_app("07-drag-and-drop")
  withr::defer(app$stop())
  out <- wait_output(app, "str", "root3")
  ## sort = FALSE keeps the order the app gave.
  expect_equal(ex_js(app, "stTop('tree')"), list("root3", "root1", "root2"))
  expect_equal(regmatches(out, gregexpr("\\$ root[0-9]", out))[[1]],
               c("$ root3", "$ root1", "$ root2"))

  ## A drop ends in jsTree's move_node, and the move alone must reach the
  ## server: the output is checked before anything else happens.
  before <- app$get_value(output = "str")
  ex_js(app, "stTree('tree').move_node(stNode('tree', 'root3'), stNode('tree', 'root2'), 'last')")
  ex_wait(app, "stTop('tree').length === 2")
  expect_equal(ex_js(app, "stKids('tree', 'root2')"), list("SubListA", "root3"))
  app$wait_for_value(output = "str", ignore = list(before))
  tree <- app$get_value(input = "tree")
  expect_equal(names(tree), c("root1", "root2"))
  expect_equal(names(tree$root2), c("SubListA", "root3"))

  ## unique = TRUE: a node cannot be dropped next to a sibling of the same name.
  ex_js(app, "stTree('tree').move_node(stNodes('tree', 'SubListA')[1], stNode('tree', 'root1'), 'last')")
  app$wait_for_idle()
  expect_equal(ex_js(app, "stKids('tree', 'root1')"), list("SubListA"))
  expect_no_js_errors(app)
})
