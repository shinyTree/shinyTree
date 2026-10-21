context("test_leaves_only")

## get_selected()/get_checked() with leaves_only = TRUE (#98). Filtering slices
## by depth only works when every branch is the same depth, so the tree below
## makes Europe one level deeper than Africa.
sel <- function(x, ...) structure(x, stselected = TRUE, ...)
world <- list(World = sel(list(
  Europe = sel(list(
    Scandinavia = sel(list(Denmark = sel(0), Sweden = sel(0))),
    France = sel(0),
    Spain = sel(0)
  )),
  Africa = sel(list(Egypt = sel(0), Morocco = sel(0)))
)))

test_that("leaves_only returns selected leaves at any depth", {
  expect_equal(unlist(get_selected(world, leaves_only = TRUE)),
               c("Denmark", "Sweden", "France", "Spain", "Egypt", "Morocco"))
})

test_that("leaves_only keeps the usual per-node information", {
  leaves <- get_selected(world, leaves_only = TRUE)
  expect_equal(attr(leaves[[1]], "ancestry"), c("World", "Europe", "Scandinavia"))
  expect_true(attr(leaves[[1]], "stselected"))

  slices <- get_selected(world, "slices", leaves_only = TRUE)
  expect_equal(slices[[1]],
               list(World = list(Europe = list(Scandinavia = list(Denmark = 0)))))
  expect_length(get_selected(world, "classid", leaves_only = TRUE), 6)
})

test_that("a selected parent with no selected children is left out", {
  tree <- list(a = sel(list(b = 0, c = 0)), d = sel(0))
  expect_equal(unlist(get_selected(tree, leaves_only = TRUE)), "d")
})

test_that("leaves_only defaults to FALSE and changes nothing by default", {
  expect_identical(get_selected(world), get_selected(world, leaves_only = FALSE))
  expect_length(get_selected(world), 10)
})

test_that("get_checked supports leaves_only too", {
  chk <- function(x) structure(x, stchecked = TRUE)
  ## With three_state, ticking both leaves also checks their parent.
  tree <- list(root = chk(list(a = chk(0), b = chk(0))), other = list(c = 0))
  expect_equal(unlist(get_checked(tree)), c("a", "b", "root"))
  expect_equal(unlist(get_checked(tree, leaves_only = TRUE)), c("a", "b"))
  expect_equal(get_checked(tree, "slices", leaves_only = TRUE)[[2]],
               list(root = list(b = 0)))
})

test_that("nothing selected gives an empty list", {
  tree <- list(a = list(b = 0))
  expect_identical(get_selected(tree, leaves_only = TRUE), list())
  expect_identical(get_checked(tree, "slices", leaves_only = TRUE), list())
})
