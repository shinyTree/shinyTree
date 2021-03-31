test_that("changing attribute for object works", {
  obj <- data.frame()
  obj <- set_attr(obj, "testAttr", "testVal")
  expect_true("testAttr" %in% names(attributes(obj)))
  expect_equal(attr(obj, "testAttr"), "testVal")
})

test_that("changing icons for file structure tree works",{
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
  expect_equal(attr(tree_with_icons$folder1$subfolder1$file1, "sttype"), "file")
  expect_equal(attr(tree_with_icons$folder1$subfolder1$file2, "sttype"), "file")
  expect_equal(attr(tree_with_icons$folder1$file3, "sttype"), "file")
  expect_equal(attr(tree_with_icons$folder2$file4, "sttype"), "file")
  expect_equal(attr(tree_with_icons$folder2$file5, "sttype"), "file")
  expect_equal(attr(tree_with_icons$folder1, "sttype"), "directory")
  expect_equal(attr(tree_with_icons$folder1$subfolder1, "sttype"), "directory")
  expect_equal(attr(tree_with_icons$folder2, "sttype"), "directory")
})
