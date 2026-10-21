context("test_large_tree")

## Rendering and reading a large tree was quadratic in its size (#83). These
## pin that the faster paths produce exactly what the original ones did.

## The original serialisation: the flat list of nodes handed straight to jsonlite.
list_json <- function(tree) {
  as.character(jsonlite::toJSON(get_flatList(tree), auto_unbox = TRUE))
}

test_that("Rlist2json matches serialising the node list directly", {
  cases <- list(
    one = list(a = ""),
    nested = list(a = "", b = list(c = "", d = list(e = ""))),
    attrs = list(
      a = structure("", stinfo = "x", stnum = 3.14159, stvec = c("p", "q"), stlist = list(k = 1)),
      b = structure(list(c = structure("", sticon = "/img.png", sttype = "t")),
                    stopened = TRUE, stdisabled = TRUE, stselected = TRUE, stchecked = TRUE)
    ),
    na_attr = list(a = structure("", stinfo = NA)),
    icon_only = list(a = structure("", sticon = "fa fa-x")),
    type_only = list(a = structure("", sttype = "file")),
    numeric_leaves = list(a = 1, b = list(c = 2L)),
    duplicates = list(a = "", a = structure("", stinfo = "second")),
    unicode = list(`Ünïcödé "q" \\ /` = "", `日本` = ""),
    deep = Reduce(function(acc, i) setNames(list(acc), paste0("n", i)), 1:50, list(leaf = ""))
  )
  for (name in names(cases)) {
    expect_identical(Rlist2json(cases[[name]]), list_json(cases[[name]]), info = name)
  }
})

test_that("trees the data-frame path cannot represent fall back to the list", {
  unnamed <- list("", "")
  na_name <- setNames(list("", ""), c("a", NA))

  expect_null(flatListToFrame(get_flatList(unnamed)))
  expect_null(flatListToFrame(get_flatList(na_name)))
  expect_null(flatListToFrame(list()))
  expect_identical(Rlist2json(unnamed), list_json(unnamed))
  expect_identical(Rlist2json(na_name), list_json(na_name))
  expect_identical(Rlist2json(list()), list_json(list()))
})

test_that("node ids follow depth-first order", {
  flat <- get_flatList(list(a = list(b = "", c = list(d = "")), e = ""))
  expect_equal(vapply(flat, `[[`, "", "text"), c("a", "b", "c", "d", "e"))
  expect_equal(vapply(flat, `[[`, "", "id"), as.character(1:5))
  expect_equal(vapply(flat, `[[`, "", "parent"), c("#", "1", "1", "3", "#"))
})

test_that("selected and checked nodes come back descendants first", {
  tree <- list(
    a = structure(list(
      b = structure(0, stselected = TRUE, stchecked = TRUE),
      c = structure(list(d = structure(0, stselected = TRUE)), stselected = TRUE)
    ), stselected = TRUE),
    e = structure(0, stchecked = TRUE)
  )
  expect_equal(unlist(get_selected(tree)), c("b", "d", "c", "a"))
  expect_equal(unlist(get_checked(tree)), c("b", "e"))
  expect_equal(attr(get_selected(tree)[[2]], "ancestry"), c("a", "c"))
  expect_equal(get_selected(tree, "slices")[[2]], list(a = list(c = list(d = 0))))
})

test_that("a tree with nothing selected returns an empty list in every format", {
  tree <- list(a = structure(0), b = list(c = structure(0)))
  for (format in c("names", "slices", "classid")) {
    expect_identical(get_selected(tree, format), list(), info = format)
    expect_identical(get_checked(tree, format), list(), info = format)
  }
})
