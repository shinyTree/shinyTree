context("test_review_fixes")

## Regression tests for problems found in a review of the whole package. Each
## failed before its fix.

node_json <- function(tree) jsonlite::fromJSON(Rlist2json(tree), simplifyVector = FALSE)

test_that("attributes are matched by their exact name, not a prefix of it", {
  nodes <- node_json(list(a = structure("", sttype_label = "Folder", sticonColor = "red",
                                        sttooltipHtml = "<b>", stselectedByDefault = TRUE)))
  expect_null(nodes[[1]]$type)
  expect_null(nodes[[1]]$icon)
  expect_null(nodes[[1]]$a_attr)
  expect_false(nodes[[1]]$state$selected)
  ## They are still passed through as data.
  expect_equal(nodes[[1]]$data$sttype_label, "Folder")
})

test_that("numbers in node data keep their full precision", {
  nodes <- node_json(list(a = structure("", stx = pi, stw = 123456.123456)))
  expect_equal(nodes[[1]]$data$stx, pi)
  expect_equal(nodes[[1]]$data$stw, 123456.123456)
})

test_that("R's own attributes on a node are not sent as data", {
  nodes <- node_json(list(a = structure(list(b = ""), class = "foo"),
                          d = data.frame(x = 1)))
  expect_length(nodes[[1]]$data, 0)
  expect_false(any(c("class", "row.names") %in% names(nodes[[3]]$data)))
})

test_that("fixIconName expands a bare name and leaves anything else alone", {
  expect_equal(fixIconName(c("file", "fa-file", "glyphicon-file")),
               c("fa fa-file", "fa fa-file", "glyphicon glyphicon-file"))
  ## Font Awesome 5+ styles, several classes, other icon sets and images.
  kept <- c("fas fa-file", "fa-solid fa-folder", "fa-brands fa-github", "fa-file fa-lg",
            "bi bi-folder", "ball.png", "/images/ball.jpg", "https://example.org/a.png",
            "fa fa-file", "glyphicon glyphicon-file")
  expect_equal(fixIconName(kept), kept)
  expect_equal(fixIconName(NA), NA_character_)
  expect_null(fixIconName(NULL))
})

test_that("updateTree and renderTree accept JSON from jsonlite::toJSON()", {
  json <- jsonlite::toJSON(list(list(id = "1", text = "a", parent = "#")), auto_unbox = TRUE)
  sent <- NULL
  session <- list(sendInputMessage = function(id, message) sent <<- message,
                  userData = new.env())
  updateTree(session, "tree", json)
  ## A plain string, which the browser parses -- a "json" object would be
  ## embedded in the message already parsed.
  expect_identical(class(sent$data), "character")
  expect_equal(jsonlite::fromJSON(sent$data)$text, "a")
})

test_that("jsonToAttr handles an empty tree", {
  ## What the browser reports once the user deletes the last node.
  expect_equal(length(jsonToAttr(list())), 0)
})

test_that("dfToTree builds branches that end at different depths", {
  ## Paths of different lengths, as a folder listing gives: a branch that ends
  ## two levels before the deepest was an error.
  df <- data.frame(`1` = c("a", "b", "c"), `2` = c("x", NA, "y"), `3` = c("q", NA, NA),
                   check.names = FALSE, stringsAsFactors = FALSE)
  tree <- dfToTree(df)
  expect_equal(tree, list(a = list(x = list(q = "")), b = "", c = list(y = "")))
  ## And back again.
  back <- treeToDf(tree)
  expect_equal(back[[1]], c("a", "b", "c"))
  expect_equal(back[[3]], c("q", NA, NA))
})

test_that("dfToTree handles empty input and a column that does not exist", {
  expect_equal(dfToTree(data.frame(a = character())), list())
  expect_error(dfToTree(data.frame(a = "x"), "nope"), "nope")
})

test_that("treeToDf and depth handle empty trees and empty branches", {
  expect_equal(depth(list()), 1L)
  expect_equal(depth(list(a = list(), b = "")), 2L)
  expect_equal(nrow(treeToDf(list())), 0)
  expect_equal(treeToDf(list(a = list(), b = ""))[[1]], c("a", "b"))
})

test_that("treeToJSON keeps the checked state and gives an empty tree for a bare root", {
  skip_if_not_installed("data.tree")
  root <- data.tree::Node$new("root")
  expect_equal(treeToJSON(root), "[]")

  child <- root$AddChild("child")
  child$state <- list(checked = TRUE)
  expect_silent(json <- treeToJSON(root))
  expect_true(jsonlite::fromJSON(json, simplifyVector = FALSE)[[1]]$state$checked)
})

test_that("treeToJSON does not take a field for another by its prefix", {
  skip_if_not_installed("data.tree")
  root <- data.tree::Node$new("root")
  child <- root$AddChild("child")
  child$textColor <- "red"
  child$iconSize <- "lg"
  node <- jsonlite::fromJSON(treeToJSON(root), simplifyVector = FALSE)[[1]]
  expect_equal(node$text, "child")
  expect_null(node$icon)
})

test_that("the parser option set before loading is kept, and NULL falls back to list", {
  old <- options(shinyTree.defaultParser = "tree")
  withr::defer(options(old))
  shinyTree:::.onLoad(NULL, "shinyTree")
  expect_equal(getOption("shinyTree.defaultParser"), "tree")

  options(shinyTree.defaultParser = NULL)
  handler <- shiny:::inputHandlers$get("shinyTree")
  expect_equal(names(handler(list(list(text = "a", id = "1")), NULL, "tree")), "a")
})

test_that("the package can be loaded again in the same session", {
  ## .onLoad registered the input handler without force, so loading it again
  ## -- as devtools::load_all() does -- failed.
  expect_silent(shinyTree:::.onLoad(NULL, "shinyTree"))
})

test_that("the tree parser warns about siblings with the same name", {
  skip_if_not_installed("data.tree")
  old <- options(shinyTree.defaultParser = "tree")
  withr::defer(options(old))
  handler <- shiny:::inputHandlers$get("shinyTree")
  val <- list(list(text = "a", id = "1"), list(text = "a", id = "2"))
  expect_warning(handler(val, NULL, "tree"), "same name")
})

test_that("shinyTree() accepts NULL search and animation, and escapes ids in scripts", {
  expect_silent(shinyTree("t", search = NULL, animation = NULL))
  html <- as.character(htmltools::renderTags(
    shinyTree("a'b", search = TRUE, types = "{}"))$html)
  expect_true(grepl('shinyTree.initSearch("a\'b", "a\'b-search-input", 250);', html, fixed = TRUE))
  expect_true(grepl('window["a\'b_sttypes"] = {};', html, fixed = TRUE))
  ## The search script is not nested inside the <input>.
  expect_false(grepl("<input[^>]*>\\s*<script", gsub("\n", " ", html)) &&
                 grepl("</input>", html))
  expect_equal(jsString("</script>"), '"<\\/script>"')
})
