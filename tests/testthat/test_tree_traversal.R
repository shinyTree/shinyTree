tree <- list(
  "folder1" = list(
    "subfolder1" = list(
      "file1" = "",
      "file2" = ""
    ),
    "file3" = ""),
  "folder2" = list(
    "file4" = "",
    "file5" = "")
)

tree_with_icons <- set_node_attrs(tree, attr_name = "sttype", inner_val = "directory", leaf_val = "file")


test_that("New attribute is in every node", {
  expect_true("sttype" %in% names(attributes(tree_with_icons$folder1)))
  expect_true("sttype" %in% names(attributes(tree_with_icons$folder1$subfolder1)))
  expect_true("sttype" %in% names(attributes(tree_with_icons$folder1$subfolder1$file1)))
  expect_true("sttype" %in% names(attributes(tree_with_icons$folder1$subfolder1$file2)))
  expect_true("sttype" %in% names(attributes(tree_with_icons$folder2)))
})

test_that("Correct attribute in every node", {
  expect_equal(attr(tree_with_icons$folder1$subfolder1$file1, "sttype"), "file")
  expect_equal(attr(tree_with_icons$folder1$subfolder1$file2, "sttype"), "file")
  expect_equal(attr(tree_with_icons$folder1$file3, "sttype"), "file")
  expect_equal(attr(tree_with_icons$folder2$file4, "sttype"), "file")
  expect_equal(attr(tree_with_icons$folder2$file5, "sttype"), "file")
  expect_equal(attr(tree_with_icons$folder1, "sttype"), "directory")
  expect_equal(attr(tree_with_icons$folder1$subfolder1, "sttype"), "directory")
  expect_equal(attr(tree_with_icons$folder2, "sttype"), "directory")
})

test_that("Adding more than 1 attribute works",{

  tree_opened <- set_node_attrs(tree_with_icons, attr_name = "stopened", inner_val = TRUE, leaf_val = TRUE)
  
  expect_setequal(c("names", "stopened", "sttype"), names(attributes(tree_opened$folder1)))
  expect_setequal(c("names", "stopened", "sttype"), names(attributes(tree_opened$folder1$subfolder1)))
  expect_setequal(c("stopened", "sttype"), names(attributes(tree_opened$folder1$subfolder1$file1)))
  expect_setequal(c("stopened", "sttype"), names(attributes(tree_opened$folder1$subfolder1$file2)))
  expect_setequal(c("names", "stopened", "sttype"), names(attributes(tree_opened$folder2)))
})
