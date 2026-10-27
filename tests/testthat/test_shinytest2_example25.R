library(shinyTree)
library(shinytest2)

test_that("the 25-duplicate-names example reports each duplicate's own data", {
  ## Guards inst/examples/25-duplicate-names against rot. Clicks node *labels*,
  ## which is how a user interacts with the tree.
  skip_on_cran()
  skip_if_not_installed("shinytest2")

  example_dir <- system.file("examples/25-duplicate-names", package = "shinyTree")
  skip_if(example_dir == "", "25-duplicate-names example not installed")

  app <- start_app(example_dir, "example-25-duplicate-names")
  withr::defer(app$stop())

  ## Both quarters render every child, repeated names included.
  labels <- jsonlite::fromJSON(app$get_js(
    "JSON.stringify(Array.from(document.querySelectorAll('#tree a.jstree-anchor')).map(function(a){return a.textContent;}))"
  ))
  expect_equal(
    labels,
    c("Q1 2026", "Invoice", "Invoice", "Receipt", "Invoice",
      "Q2 2026", "Invoice", "Invoice")
  )

  ## Check the 2nd Invoice in Q1, the Receipt, and the 2nd Invoice in Q2 --
  ## none of them first or last within their parent.
  for (id in c("3_anchor", "4_anchor", "8_anchor")) {
    app$click(selector = sprintf("[id=\"%s\"]", id))
    app$wait_for_idle()
  }

  checked <- app$get_value(output = "checked_info")
  expect_match(checked, "INV-002 / \\$3,450 / Globex", fixed = FALSE)
  expect_match(checked, "RCP-900 / \\$99 / Initech", fixed = FALSE)
  expect_match(checked, "INV-005 / \\$2,310 / Wayne", fixed = FALSE)
  ## The unchecked siblings' data must not leak in.
  expect_false(grepl("INV-001", checked))
  expect_false(grepl("INV-004", checked))

  ## Each line names its own quarter.
  expect_match(checked, "Invoice  \\| Q1 2026  \\| INV-002", fixed = FALSE)
  expect_match(checked, "Invoice  \\| Q2 2026  \\| INV-005", fixed = FALSE)

  ## Unticking one duplicate drops only that one.
  app$click(selector = "[id=\"3_anchor\"]")
  app$wait_for_idle()
  checked <- app$get_value(output = "checked_info")
  expect_false(grepl("INV-002", checked))
  expect_match(checked, "RCP-900", fixed = TRUE)
  expect_match(checked, "INV-005", fixed = TRUE)

  ## The server receives all duplicates.
  counts <- app$get_value(output = "node_count")
  expect_match(counts, "Q1 2026 - 4 children: Invoice, Invoice, Receipt, Invoice")
  expect_match(counts, "Q2 2026 - 2 children: Invoice, Invoice")
})
