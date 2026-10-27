## Helpers for the tests of the example apps (test_shinytest2_example*.R).
##
## The installed copy of each example is started, as R CMD check runs the
## tests against the installed package.

example_app <- function(name, ...) {
  dir <- system.file("examples", name, package = "shinyTree")
  testthat::skip_if(dir == "", paste(name, "example not installed"))
  start_app(dir, paste0("example-", gsub("[^A-Za-z0-9]+", "-", name)), ...)
}

## JavaScript helpers, defined afresh for each call so they are there however
## the page has been reloaded:
##   stTree(id)        the jsTree instance of a tree
##   stNode(id, text)  the id of the first node labelled text, wherever it is
##   stNodes(id, text) the ids of every node labelled text
##   stKids(id, text)  the labels of that node's children, in order
##   stTop(id)         the labels of the top-level nodes, in order
##   stAnchor(id, text) the node's <a>, if it is rendered
example_js_helpers <- "
  var stTree = function(id) { return $.jstree.reference(document.getElementById(id)); };
  var stNodes = function(id, text) {
    var t = stTree(id);
    return Object.keys(t._model.data).filter(function(k) {
      return k !== '#' && t.get_node(k).text === text;
    });
  };
  var stNode = function(id, text) { return stNodes(id, text)[0] || null; };
  var stLabels = function(id, ids) { var t = stTree(id); return ids.map(function(k) { return t.get_node(k).text; }); };
  var stKids = function(id, text) { return stLabels(id, stTree(id).get_node(stNode(id, text)).children); };
  var stTop = function(id) { return stLabels(id, stTree(id).get_node('#').children); };
  // Looked up inside the tree: jsTree numbers the nodes of every tree from 1,
  // so a page with two trees has two elements with each id.
  var stAnchor = function(id, text) {
    var n = stNode(id, text);
    return n === null ? null : document.getElementById(id).querySelector('[id=\\'' + n + '_anchor\\']');
  };
"

## Evaluate a JavaScript expression with the helpers above; returns its value.
ex_js <- function(app, expr) {
  app$get_js(paste0("(function(){", example_js_helpers, "return (", expr, ");})()"))
}

## Wait until a JavaScript expression, with the helpers, is true.
ex_wait <- function(app, expr, timeout = 20000) {
  app$wait_for_js(paste0("(function(){", example_js_helpers,
                         "try { return !!(", expr, "); } catch (e) { return false; }})()"),
                  timeout = timeout)
}

## A CSS selector for a node's anchor, for app$click(): a real click.
node_anchor <- function(app, tree, text) {
  id <- ex_js(app, sprintf("stNode(%s, %s)", jsonlite::toJSON(tree, auto_unbox = TRUE),
                           jsonlite::toJSON(text, auto_unbox = TRUE)))
  testthat::expect_false(is.null(id), info = paste("no node", text, "in", tree))
  ## Scoped to the tree: jsTree numbers nodes from 1 in every tree, so two
  ## trees on a page have nodes with the same id.
  sprintf('[id="%s"] [id="%s_anchor"]', tree, id)
}
node_checkbox <- function(app, tree, text) paste(node_anchor(app, tree, text), ".jstree-checkbox")

## Wait for an output to match a regular expression; returns its text.
wait_output <- function(app, output, pattern, timeout = 20000) {
  app$wait_for_js(sprintf(
    "(function(){ var el = document.getElementById(%s); return !!el && new RegExp(%s).test(el.textContent); })()",
    jsonlite::toJSON(output, auto_unbox = TRUE), jsonlite::toJSON(pattern, auto_unbox = TRUE)),
    timeout = timeout)
  app$get_text(paste0("#", output))
}

## No JavaScript errors in the browser since the app started, including those
## Shiny catches in a binding and only logs.
expect_no_js_errors <- function(app) {
  logs <- app$get_logs()
  errors <- logs[logs$location == "chromote" & logs$level %in% c("error", "throw"), "message",
                 drop = TRUE]
  errors <- errors[!grepl("Failed to load resource|favicon", errors)]
  testthat::expect_equal(errors, character(0))
}
