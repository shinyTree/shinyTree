## The fixture for test_tree_traversal.R, built here so an error in it is
## reported as a helper error rather than half-way through the file.
traversal_tree <- list(
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

tree_with_icons <- set_node_attrs(traversal_tree, attr_name = "sttype", inner_val = "directory", leaf_val = "file")
