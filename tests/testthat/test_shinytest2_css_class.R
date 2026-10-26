library(shinytest2)

test_that("stcssclass puts CSS classes on a node (#58)", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")

  app <- start_app(test_path("apps/test_css_class"), "shinytree-css-class")
  withr::defer(app$stop())

  ## Each node's label, its own classes (not jsTree's) and how it is styled.
  nodes <- function() {
    jsonlite::fromJSON(app$get_js(
      "JSON.stringify(Array.from(document.querySelectorAll('#tree li')).map(function(li){
         var a = li.querySelector(':scope > a'), cs = getComputedStyle(a);
         return [a.textContent,
                 Array.from(li.classList).filter(function(c){ return c.indexOf('jstree') !== 0; }).join(' '),
                 cs.color, cs.fontWeight];
       }))"
    ))
  }

  app$wait_for_js("$('#tree li').length === 5")
  n <- nodes()
  expect_equal(n[, 1], c("projects", "alpha", "beta", "gamma", "delta"))
  expect_equal(n[, 2], c("group", "archived urgent", "urgent", "", ""))
  ## The app's CSS applies: archived is grey, urgent is bold.
  expect_equal(n[2, 3], "rgb(128, 128, 128)")
  expect_equal(n[2:3, 4], c("700", "700"))
  expect_equal(n[4, 4], "400")

  ## jsTree rebuilds a node's <li> when it is redrawn; the classes are kept.
  app$run_js("var t = $('#tree').jstree(true); t.close_all(); t.open_all();")
  app$wait_for_js("$('#tree li:visible').length === 5")
  expect_equal(nodes()[, 2], n[, 2])
})
