context("test_duplicate_names")

## Adversarial regression tests for issue #104:
## "get_selected not working for duplicated entries"
##
## shinyTree represents trees as named nested lists. R lists permit duplicate
## names, and every get_selected_*/get_checked_* traversal walks by index
## (names(tree)[i]), so duplicate sibling names are representable end-to-end.
## Two sites keyed lookups by name instead of index and lost data:
##   * jsonToAttr()   (inbound)  -- ret[[text]] <- ... overwrote same-named siblings
##   * get_flatList() (outbound) -- nestedList[[name]] always resolved to the
##                                  first match, smearing its attributes onto
##                                  every later sibling sharing that name

make_node <- function(text, selected = FALSE, checked = FALSE, disabled = FALSE,
                      opened = FALSE, data = list(), id = NULL) {
  node <- list(
    text = text,
    children = list(),
    state = list(opened = opened, selected = selected, disabled = disabled, checked = checked),
    data = data
  )
  if (!is.null(id)) node$id <- id
  node
}

test_that("jsonToAttr keeps every duplicate-named sibling (inbound)", {
  json <- list(
    make_node("leafA", id = "1"),
    make_node("leafA", selected = TRUE, id = "2"),
    make_node("leafA", id = "3"),
    make_node("leafB", id = "4"),
    make_node("leafA", id = "5")
  )

  res <- shinyTree:::jsonToAttr(json)

  expect_length(res, 5)
  expect_equal(names(res), c("leafA", "leafA", "leafA", "leafB", "leafA"))
  ## each sibling keeps its own identity rather than collapsing onto the last one
  expect_equal(unname(sapply(res, attr, "id")), as.character(1:5))
})

test_that("get_selected finds a selected duplicate-named node that is not the last sibling", {
  ## Mirrors the reporter's tree: four leafA siblings, the second one selected.
  json <- list(
    make_node("leafA", data = list(stinfo = "Info LeafA1"), id = "1"),
    make_node("leafA", selected = TRUE, data = list(stinfo = "Info LeafA2"), id = "2"),
    make_node("leafA", data = list(stinfo = "Info LeafA3"), id = "3"),
    make_node("leafB", data = list(stinfo = "Info LeafB"), id = "4"),
    make_node("leafA", data = list(stinfo = "Info LeafA4"), id = "5")
  )
  tree <- shinyTree:::jsonToAttr(json)

  sel <- get_selected(tree)
  expect_length(sel, 1)
  expect_equal(sel[[1]][1], "leafA")
  ## the *correct* duplicate must be reported, carrying its own stinfo
  expect_equal(attr(sel[[1]], "stinfo"), "Info LeafA2")
})

test_that("get_selected and get_checked report every selected/checked duplicate sibling", {
  json <- list(
    make_node("dup", selected = TRUE, checked = TRUE, data = list(stinfo = "first"), id = "1"),
    make_node("dup", data = list(stinfo = "second"), id = "2"),
    make_node("dup", selected = TRUE, checked = TRUE, data = list(stinfo = "third"), id = "3")
  )
  tree <- shinyTree:::jsonToAttr(json)

  sel <- get_selected(tree)
  expect_length(sel, 2)
  expect_equal(sapply(sel, attr, "stinfo"), c("first", "third"))

  chk <- get_checked(tree)
  expect_length(chk, 2)
  expect_equal(sapply(chk, attr, "stinfo"), c("first", "third"))
})

test_that("duplicate-named siblings keep their ancestry through nested levels", {
  parent <- make_node("SubListB", id = "9")
  parent$children <- list(
    make_node("leafA", id = "10"),
    make_node("leafA", selected = TRUE, id = "11")
  )
  tree <- shinyTree:::jsonToAttr(list(parent))

  sel <- get_selected(tree)
  expect_length(sel, 1)
  expect_equal(sel[[1]][1], "leafA")
  expect_equal(attr(sel[[1]], "ancestry"), "SubListB")
})

test_that("get_flatList gives each duplicate-named sibling its own attributes (outbound)", {
  li <- list(
    SubListB = list(
      leafA = structure("", stinfo = "Info-A1", stselected = TRUE),
      leafA = structure("", stinfo = "Info-A2"),
      leafA = structure("", stinfo = "Info-A3")
    )
  )

  res <- shinyTree:::get_flatList(li)
  expect_length(res, 4)

  leaves <- res[sapply(res, function(n) n$text == "leafA")]
  expect_length(leaves, 3)

  ## attributes must not be smeared from the first sibling onto the others
  expect_equal(unname(sapply(leaves, function(n) n$data$stinfo)),
               c("Info-A1", "Info-A2", "Info-A3"))
  expect_equal(unname(sapply(leaves, function(n) n$state$selected)),
               c(TRUE, FALSE, FALSE))
})

test_that("a tree with duplicate-named siblings round-trips through Rlist2json", {
  li <- list(
    SubListB = list(
      leafA = structure("", sticon = "fa fa-file"),
      leafA = structure(""),
      leafB = structure("")
    )
  )

  js <- shinyTree:::Rlist2json(li)
  expect_true(jsonlite::validate(js))

  parsed <- jsonlite::fromJSON(js, simplifyVector = FALSE)
  expect_equal(unname(sapply(parsed, function(n) n$text)),
               c("SubListB", "leafA", "leafA", "leafB"))
  ## only the first leafA carries the icon
  icons <- sapply(parsed, function(n) if (is.null(n$icon)) NA_character_ else n$icon)
  expect_equal(unname(icons), c(NA, "fa fa-file", NA, NA))
})

test_that("trees with unique sibling names are unaffected by the duplicate-name handling", {
  ## Backward-compatibility guard: the index-based walks in jsonToAttr() and
  ## get_flatList() must produce exactly what the name-keyed versions did for
  ## any tree whose siblings are uniquely named -- names, order, attributes,
  ## node ids and parent links all included.
  json <- list(
    make_node("root1", selected = TRUE, data = list(stinfo = "A"), id = "1"),
    make_node("root2", id = "2")
  )
  json[[2]]$children <- list(
    make_node("SubListA", opened = TRUE, id = "3"),
    make_node("SubListB", id = "4")
  )
  json[[2]]$children[[1]]$children <- list(
    make_node("leaf1", checked = TRUE, id = "5"),
    make_node("leaf2", disabled = TRUE, id = "6")
  )

  parsed <- shinyTree:::jsonToAttr(json)
  expect_equal(names(parsed), c("root1", "root2"))
  expect_equal(names(parsed$root2), c("SubListA", "SubListB"))
  expect_equal(names(parsed$root2$SubListA), c("leaf1", "leaf2"))
  expect_true(attr(parsed$root1, "stselected"))
  expect_equal(attr(parsed$root1, "stinfo"), "A")
  expect_true(attr(parsed$root2$SubListA$leaf1, "stchecked"))
  expect_true(attr(parsed$root2$SubListA$leaf2, "stdisabled"))
  expect_equal(get_selected(parsed)[[1]][1], "root1")
  expect_equal(get_checked(parsed)[[1]][1], "leaf1")

  li <- list(
    root1 = structure("", stselected = TRUE, sticon = "fa-file"),
    root2 = list(
      SubListA = structure(list(leaf1 = "", leaf2 = ""), stopened = TRUE),
      SubListB = list(leafA = "")
    )
  )
  flat <- shinyTree:::get_flatList(li)
  expect_equal(unname(sapply(flat, function(n) n$text)),
               c("root1", "root2", "SubListA", "leaf1", "leaf2", "SubListB", "leafA"))
  expect_equal(unname(sapply(flat, function(n) n$id)), as.character(1:7))
  expect_equal(unname(sapply(flat, function(n) n$parent)),
               c("#", "#", "2", "3", "3", "2", "6"))
  expect_true(flat[[1]]$state$selected)
  expect_equal(flat[[1]]$icon, "fa fa-file")
  expect_true(flat[[3]]$state$opened)
})
