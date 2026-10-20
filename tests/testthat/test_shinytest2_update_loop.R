library(shinyTree)
library(shinytest2)

## End-to-end tests for issue #36: updateTree() must not echo its own refresh
## back to the server, while every user-initiated event must still arrive.
##
## The event map below was established empirically by attaching listeners to all
## 18 events shinyTree subscribes to and recording which fired for each trigger:
##
##   server updateTree()        -> refresh, set_state      (suppressed)
##   select / deselect a node   -> changed
##   open / close a node        -> open_node / close_node
##   drag a node                -> move_node
##   contextmenu create/rename/delete -> create_node / rename_node / delete_node
##   checkbox check/uncheck/all -> check_node / uncheck_node / check_all / uncheck_all
##
## Nothing user-initiated emits refresh or set_state, which is what makes the
## suppression safe.

## Counts input$tree updates by reading the app's own counter output.
updates <- function(app) as.integer(app$get_value(output = "updates"))

start_app <- function(dir, name) {
  app <- tryCatch(
    AppDriver$new(app_dir = test_path(dir), name = name, variant = NULL,
                  load_timeout = 30000),
    error = function(e) e
  )
  if (inherits(app, "error")) {
    skip(paste("AppDriver could not be started:", conditionMessage(app)))
  }
  app
}

test_that("a naive observe + updateTree pattern does not loop (#36)", {
  ## The adversarial case. Both callback options are on and the observer calls
  ## updateTree() in response to input$tree -- the pattern the issue describes.
  ## The app caps itself at 50 iterations so a runaway terminates and can be
  ## asserted on rather than hanging the suite. Before the fix this hit the cap
  ## on page load alone, with no interaction at all.
  skip_on_cran()
  skip_if_not_installed("shinytest2")

  app <- start_app("apps/test_update_loop", "loop-no-runaway")
  withr::defer(app$stop())

  ## Give any feedback loop several seconds of wall clock to run away.
  Sys.sleep(3)
  app$wait_for_idle()

  expect_equal(app$get_value(output = "capped"), "bounded")
  expect_lt(updates(app), 10)

  ## An explicit server-side update must also not start the loop.
  before <- updates(app)
  app$click(selector = "#server_update")
  Sys.sleep(2)
  app$wait_for_idle()

  expect_equal(app$get_value(output = "capped"), "bounded")
  expect_equal(updates(app), before)
})

test_that("a server-initiated updateTree does not notify the server (#36)", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")

  app <- start_app("apps/test_events_all", "events-server-refresh")
  withr::defer(app$stop())

  ## Establish a baseline with one real user event so the counter is non-zero
  ## and we are comparing like with like.
  app$click(selector = "[id=\"1_anchor\"]")
  app$wait_for_idle()
  baseline <- updates(app)
  expect_gt(baseline, 0)

  ## The refresh and set_state events both fire here -- verified below -- but
  ## neither may reach the server.
  app$click(selector = "#server_update")
  Sys.sleep(2)
  app$wait_for_idle()
  expect_equal(updates(app), baseline)

  ## Prove the events really did fire, so this test cannot pass by accident if
  ## updateTree() silently stopped refreshing the tree.
  fired <- app$get_js("(function(){
    var el = document.getElementById('tree');
    window.__seen = [];
    $(el).on('refresh.jstree set_state.jstree', function(e){ window.__seen.push(e.type); });
    return 'armed';
  })()")
  expect_equal(fired, "armed")

  app$click(selector = "#server_update")
  Sys.sleep(2)
  app$wait_for_idle()

  seen <- jsonlite::fromJSON(app$get_js("JSON.stringify(window.__seen)"))
  expect_true("refresh" %in% seen)
  expect_true("set_state" %in% seen)
  ## ... and still no notification despite the events firing.
  expect_equal(updates(app), baseline)
})

test_that("selecting and deselecting a node notifies the server (changed)", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")

  app <- start_app("apps/test_events_all", "events-changed")
  withr::defer(app$stop())

  before <- updates(app)
  app$click(selector = "[id=\"1_anchor\"]")
  app$wait_for_idle()
  after_select <- updates(app)
  expect_gt(after_select, before)

  ## Ctrl-click to deselect, which fires changed again.
  app$run_js("$('#tree').jstree(true).deselect_node('1');")
  app$wait_for_idle()
  expect_gt(updates(app), after_select)
})

test_that("opening and closing a node notifies the server", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")

  app <- start_app("apps/test_events_all", "events-open-close")
  withr::defer(app$stop())

  ## root2 starts open, so close then open it.
  before <- updates(app)
  app$run_js("$('#tree').jstree(true).close_node('2');")
  app$wait_for_idle()
  after_close <- updates(app)
  expect_gt(after_close, before)

  app$run_js("$('#tree').jstree(true).open_node('2');")
  app$wait_for_idle()
  expect_gt(updates(app), after_close)
})

test_that("moving a node notifies the server (move_node)", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")

  app <- start_app("apps/test_events_all", "events-move")
  withr::defer(app$stop())

  before <- updates(app)
  app$run_js("$('#tree').jstree(true).move_node('3', '#');")
  app$wait_for_idle()
  expect_gt(updates(app), before)
})

test_that("contextmenu create, rename and delete notify the server", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")

  app <- start_app("apps/test_events_all", "events-contextmenu")
  withr::defer(app$stop())

  before <- updates(app)
  app$run_js("window.__newId = $('#tree').jstree(true).create_node('2', {text: 'created'});")
  app$wait_for_idle()
  after_create <- updates(app)
  expect_gt(after_create, before)

  app$run_js("$('#tree').jstree(true).rename_node(window.__newId, 'renamed');")
  app$wait_for_idle()
  after_rename <- updates(app)
  expect_gt(after_rename, after_create)

  app$run_js("$('#tree').jstree(true).delete_node(window.__newId);")
  app$wait_for_idle()
  expect_gt(updates(app), after_rename)
})

test_that("checkbox check, uncheck, check_all and uncheck_all notify the server", {
  ## Separate app: shinyTree() force-disables checkbox when contextmenu is on,
  ## and these four events only fire when tie_selection is FALSE.
  skip_on_cran()
  skip_if_not_installed("shinytest2")

  app <- start_app("apps/test_events_checkbox", "events-checkbox")
  withr::defer(app$stop())

  expect_equal(app$get_js("document.getElementById('tree').dataset.stCheckbox"), "TRUE")
  expect_equal(app$get_js("document.getElementById('tree').dataset.stTieSelection"), "FALSE")

  before <- updates(app)
  app$run_js("$('#tree').jstree(true).check_node('1');")
  app$wait_for_idle()
  after_check <- updates(app)
  expect_gt(after_check, before)

  app$run_js("$('#tree').jstree(true).uncheck_node('1');")
  app$wait_for_idle()
  after_uncheck <- updates(app)
  expect_gt(after_uncheck, after_check)

  app$run_js("$('#tree').jstree(true).check_all();")
  app$wait_for_idle()
  after_check_all <- updates(app)
  expect_gt(after_check_all, after_uncheck)

  app$run_js("$('#tree').jstree(true).uncheck_all();")
  app$wait_for_idle()
  expect_gt(updates(app), after_check_all)
})

test_that("a server update does not suppress a later user event (#36)", {
  ## The suppression must be scoped to the server's own refresh, not latch on
  ## and mute the tree afterwards.
  skip_on_cran()
  skip_if_not_installed("shinytest2")

  app <- start_app("apps/test_events_all", "events-not-latched")
  withr::defer(app$stop())

  app$click(selector = "#server_update")
  Sys.sleep(2)
  app$wait_for_idle()
  after_server <- updates(app)

  ## A user click immediately after the server update must still be reported.
  app$click(selector = "[id=\"1_anchor\"]")
  app$wait_for_idle()
  expect_gt(updates(app), after_server)

  ## And the flag must have been cleared rather than left set.
  expect_false(app$get_js("!!document.getElementById('tree').stServerRefresh"))
})

test_that("a rejected change can be repeated and still reaches the server (#34)", {
  ## Regression guard for the fix this change is most likely to break.
  ## The server rejects any selection of root2 and restores the tree. The user
  ## then makes the *same* selection again: because the payload is identical,
  ## Shiny would discard it as a duplicate were it not for the callbackCounter.
  ## Suppressing the updateTree() echo must not re-break that.
  skip_on_cran()
  skip_if_not_installed("shinytest2")

  app <- start_app("apps/test_reject_change", "reject-repeat")
  withr::defer(app$stop())

  expect_equal(as.integer(app$get_value(output = "rejections")), 0)

  for (attempt in 1:3) {
    app$click(selector = "[id=\"2_anchor\"]")
    app$wait_for_idle()
    Sys.sleep(0.5)
    expect_equal(as.integer(app$get_value(output = "rejections")), attempt)
  }
})
