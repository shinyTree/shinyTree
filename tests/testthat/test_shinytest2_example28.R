library(shinyTree)
library(shinytest2)

test_that("the 28-ontology example shows the AEO and searches it (#33)", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")

  example_dir <- system.file("examples/28-ontology", package = "shinyTree")
  skip_if(example_dir == "", "28-ontology example not installed")

  app <- start_app(example_dir, "example-28-ontology")
  withr::defer(app$stop())

  app$wait_for_js("$('#tree li').length > 0", timeout = 15000)
  expect_equal(app$get_value(output = "summary"), "250 terms, 1 obsolete")
  top <- jsonlite::fromJSON(app$get_js(
    "JSON.stringify(Array.from(document.querySelectorAll('#tree > ul > li > a')).map(function(a){ return a.textContent; }))"))
  expect_equal(top, c("anatomical entity", "cell", "obsolete"))
  ## The definition is the hover text.
  expect_match(app$get_js("$('#tree > ul > li > a').first().attr('title')"), "^CARO:0000000: Biological entity")

  ## Search reaches a term several levels down, which is then selectable.
  app$run_js("$('#tree-search-input').val('lumen of epithelial sac').trigger('keyup')")
  app$wait_for_js("Array.from(document.querySelectorAll('#tree .jstree-search')).some(function(a){ return a.textContent === 'lumen of epithelial sac'; })",
                  timeout = 15000)
  app$run_js("var t = $('#tree').jstree(true); t.select_node($('#tree .jstree-search').first().closest('li').attr('id'));")
  app$wait_for_idle()
  expect_match(app$get_value(output = "selected"), "AEO:0000079 lumen of epithelial sac", fixed = TRUE)
})

test_that("28-ontology: a term is under each of its parents; search by id; upload", {
  skip_if_not_installed("xml2")
  app <- example_app("28-ontology")
  withr::defer(app$stop())
  ex_wait(app, "stTop('tree').length === 3")
  expect_equal(app$get_js("$('#tree-search-input').attr('placeholder')"), "Search by name or id")

  ## sclerotome's one parent; "developing mesenchymal condensation" has two
  ## and is shown under each.
  app$run_js("$('#tree-search-input').val('AEO:0000148').trigger('input')")
  ex_wait(app, "$('#tree a.jstree-search').length === 2", timeout = 30000)
  under <- unlist(ex_js(app, "$('#tree a.jstree-search').map(function(){ return stTree('tree').get_node(stTree('tree').get_parent($(this).closest('li').attr('id'))).text; }).get().sort()"))
  expect_equal(under, c("dense mesenchyme tissue", "developing mesenchymal structure"))
  ## Each place is the same term, with the same id.
  ids <- unlist(ex_js(app, "$('#tree a.jstree-search').map(function(){ return stTree('tree').get_node($(this).closest('li').attr('id')).data.stid; }).get()"))
  expect_equal(ids, c("AEO:0000148", "AEO:0000148"))

  ## Obsolete terms are greyed out, under their own node.
  ex_js(app, "stTree('tree').open_node(stNode('tree', 'obsolete'))")
  ex_wait(app, "stKids('tree', 'obsolete').length === 1")
  expect_true(ex_js(app, "stTree('tree').is_disabled(stTree('tree').get_node(stNode('tree', 'obsolete')).children[0])"))

  ## Uploading the same ontology gives the same tree; a wrong file says so.
  app$set_inputs(source = "upload")
  app$upload_file(file = system.file("examples/28-ontology/aeo.obo", package = "shinyTree"))
  ex_wait(app, "stTop('tree').length === 3")
  expect_equal(app$get_value(output = "summary"), "250 terms, 1 obsolete")
  bad <- withr::local_tempfile(fileext = ".txt")
  writeLines("not an ontology", bad)
  app$upload_file(file = bad)
  app$wait_for_js("/\\.obo or \\.owl/.test(document.body.textContent)")
  expect_no_js_errors(app)
})
