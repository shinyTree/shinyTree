context("test_get_checked")

test_that("get_checked_names", {
  ## No Checked ##############
  tree <- list(
    root1 = structure("123"),
    root2 = list(
      SubListA = list(leaf1 = "", leaf2 = "", leaf3 = ""),
      SubListB = list(leafA = "", leafB = "")
    )
  )
  chk <- get_checked(tree)
  expect_is(chk, "list")
  expect_length(chk, 0)

  ## 1 Checked ( root1 ) ##############
  tree <- list(
    root1 = structure("123", stchecked = T),
    root2 = list(
      SubListA = list(leaf1 = "", leaf2 = "", leaf3 = ""),
      SubListB = list(leafA = "", leafB = "")
    )
  )
  chk <- get_checked(tree)
  expect_is(chk, "list")
  expect_length(chk, 1)
  expect_true(chk[[1]][1] == "root1")
  expect_true(length(attr(chk[[1]], "ancestry")) == 0)
  expect_true(attr(chk[[1]], "stchecked"))

  ## 1 Checked ( leaf1 ) ##############
  tree <- list(
    root1 = structure("123"),
    root2 = list(
      SubListA = list(leaf1 = structure("", stchecked = TRUE), leaf2 = "", leaf3 = ""),
      SubListB = list(leafA = "", leafB = "")
    )
  )
  chk <- get_checked(tree)
  expect_is(chk, "list")
  expect_length(chk, 1)
  expect_true(chk[[1]][1] == "leaf1")
  expect_true(all(attr(chk[[1]], "ancestry") == c("root2", "SubListA")))
  expect_true(attr(chk[[1]], "stchecked"))

  ## 2 Checked ( leaf1 / leafB ) ##############
  tree <- list(
    root1 = structure("123"),
    root2 = list(
      SubListA = list(leaf1 = structure("", stchecked = TRUE), leaf2 = "", leaf3 = ""),
      SubListB = list(leafA = structure("", stchecked = TRUE), leafB = "")
    )
  )
  chk <- get_checked(tree)
  expect_is(chk, "list")
  expect_length(chk, 2)
  expect_true(chk[[1]][1] == "leaf1")
  expect_true(all(attr(chk[[1]], "ancestry") == c("root2", "SubListA")))
  expect_true(attr(chk[[1]], "stchecked"))
  expect_true(chk[[2]][1] == "leafA")
  expect_true(all(attr(chk[[2]], "ancestry") == c("root2", "SubListB")))
  expect_true(attr(chk[[2]], "stchecked"))
})

test_that("get_checked_slices", {
  ## No Checked ##############
  tree <- list(
    root1 = structure("123"),
    root2 = list(
      SubListA = list(leaf1 = "", leaf2 = "", leaf3 = ""),
      SubListB = list(leafA = "", leafB = "")
    )
  )
  chk <- get_checked(tree, "slices")
  expect_is(chk, "list")
  expect_length(chk, 0)

  ## 1 Checked ( root1 ) ##############
  tree <- list(
    root1 = structure("123", stchecked = T),
    root2 = list(
      SubListA = list(leaf1 = "", leaf2 = "", leaf3 = ""),
      SubListB = list(leafA = "", leafB = "")
    )
  )
  chk <- get_checked(tree, "slices")
  expect_is(chk, "list")
  expect_length(chk, 1)
  expect_true(names(chk[[1]]) == "root1")

  ## 1 Checked ( leaf1 ) ##############
  tree <- list(
    root1 = structure("123"),
    root2 = list(
      SubListA = list(leaf1 = structure("", stchecked = TRUE), leaf2 = "", leaf3 = ""),
      SubListB = list(leafA = "", leafB = "")
    )
  )
  chk <- get_checked(tree, "slices")
  expect_is(chk, "list")
  expect_length(chk, 1)
  expect_true(names(chk[[1]]) == "root2")
  expect_true(names(chk[[1]][[1]]) == "SubListA")
  expect_true(names(chk[[1]][[1]][[1]]) == "leaf1")

  ## 2 Checked ( leaf1 / leafB ) ##############
  tree <- list(
    root1 = structure("123"),
    root2 = list(
      SubListA = list(leaf1 = structure("", stchecked = TRUE), leaf2 = "", leaf3 = ""),
      SubListB = list(leafA = structure("", stchecked = TRUE), leafB = "")
    )
  )
  chk <- get_checked(tree, "slices")
  expect_is(chk, "list")
  expect_length(chk, 2)
  expect_true(names(chk[[1]]) == "root2")
  expect_true(names(chk[[1]][[1]]) == "SubListA")
  expect_true(names(chk[[1]][[1]][[1]]) == "leaf1")
  expect_true(names(chk[[2]]) == "root2")
  expect_true(names(chk[[2]][[1]]) == "SubListB")
  expect_true(names(chk[[2]][[1]][[1]]) == "leafA")
})

test_that("get_checked_classid", {
  ## No Checked ##############
  tree <- list(
    root1 = structure("123"),
    root2 = list(
      SubListA = list(leaf1 = "", leaf2 = "", leaf3 = ""),
      SubListB = list(leafA = "", leafB = "")
    )
  )
  chk <- get_checked(tree, "classid")
  expect_is(chk, "list")
  expect_length(chk, 0)

  ## 1 Checked ( root1 ) ##############
  tree <- list(
    root1 = structure("123", stchecked = T, stclass = "my-class", stid = "my-id", id = "1"),
    root2 = list(
      SubListA = list(leaf1 = "", leaf2 = "", leaf3 = ""),
      SubListB = list(leafA = "", leafB = "")
    )
  )
  chk <- get_checked(tree, "classid")
  expect_is(chk, "list")
  expect_length(chk, 1)
  expect_true(chk[[1]] == "root1")
  expect_equal(attr(chk[[1]], "stclass"), "my-class")
  expect_equal(attr(chk[[1]], "stid"), "my-id")
  expect_equal(attr(chk[[1]], "id"), "1")

  ## 1 Checked ( leaf1 ) ##############
  tree <- list(
    root1 = structure("123"),
    root2 = list(
      SubListA = list(leaf1 = structure("", stchecked = TRUE), leaf2 = "", leaf3 = ""),
      SubListB = list(leafA = "", leafB = "")
    )
  )
  chk <- get_checked(tree, "classid")
  expect_is(chk, "list")
  expect_length(chk, 1)
  expect_true(chk[[1]] == "leaf1")

  ## 2 Checked ( leaf1 / leafA ) ##############
  tree <- list(
    root1 = structure("123"),
    root2 = list(
      SubListA = list(leaf1 = structure("", stchecked = TRUE), leaf2 = "", leaf3 = ""),
      SubListB = list(leafA = structure("", stchecked = TRUE), leafB = "")
    )
  )
  chk <- get_checked(tree, "classid")
  expect_is(chk, "list")
  expect_length(chk, 2)
  expect_true(chk[[1]] == "leaf1")
  expect_true(chk[[2]] == "leafA")
})

test_that("get_checked dispatches via format argument and errors on bad format", {
  tree <- list(root1 = structure("123", stchecked = TRUE))

  expect_equal(get_checked(tree, "names"), shinyTree:::get_checked_names(tree))
  expect_equal(get_checked(tree, "slices"), shinyTree:::get_checked_slices(tree))
  expect_equal(get_checked(tree, "classid"), shinyTree:::get_checked_classid(tree))
  expect_error(get_checked(tree, "bogus"))
})
