context("test_tree")
library(shiny)
library(jsonlite)

make_data_tree <- function() {
  tree <- data.tree::Node$new("myroot")
  a <- tree$AddChild("A")
  a$AddChild("A1")
  tree$AddChild("B")
  tree
}


test_that("get_flatList", {  
  li <- list(abc = 123, def = 456)
  res <- shinyTree:::get_flatList(li)
  expect_is(res, "list")
  expect_length(res, 2)
  expect_true(all(unlist(lapply(res, class)) == "list"))
  expect_true(all(unlist(lapply(res, length)) == 5))
  expect_true(res[[1]]$text == "abc" && res[[2]]$text == "def")
  expect_true(all(unlist(res[[1]]$state) == FALSE))
  
  ## example 01 #################
  li <- list(
    root1 = "",
    root2 = list(
      SubListA = list(leaf1 = "", leaf2 = "", leaf3=""),
      SubListB = list(leafA = "", leafB = "")
    ),
    root3 = list(
      SubListA = list(leaf1 = "", leaf2 = "", leaf3=""),
      SubListB = list(leafA = "", leafB = "")
    )
  )
  res <- shinyTree:::get_flatList(li)
  expect_is(res, "list")
  expect_length(res, 17)
  expect_true(all(unlist(lapply(res, class)) == "list"))
  expect_true(all(unlist(lapply(res, length)) == 5))
  
  ## example 01 with custom attribute #################
  li <- list(
    root1 = structure("123", stselected = T, stsomethingelse = T),
    root2 = list(
      SubListA = list(leaf1 = "", leaf2 = "", leaf3=""),
      SubListB = list(leafA = "", leafB = "")
    )
  )
  res <- shinyTree:::get_flatList(li)
  expect_is(res, "list")
  expect_length(res, 9)
  expect_true(all(unlist(lapply(res, class)) == "list"))
  expect_true(all(unlist(lapply(res, length)) == 5))
})

test_that("renderTree", {  
  res <- renderTree({
    list(
      root1 = "",
      root2 = list(
        SubListA = list(leaf1 = "", leaf2 = "", leaf3=""),
        SubListB = list(leafA = "", leafB = "")
      ),
      root3 = list(
        SubListA = list(leaf1 = "", leaf2 = "", leaf3=""),
        SubListB = list(leafA = "", leafB = "")
      )
    )
  })
  expect_is(res, "function")
})
test_that("renderTreeAsync", {  
  res <- renderTreeAsync({
    list(
      root1 = "",
      root2 = list(
        SubListA = list(leaf1 = "", leaf2 = "", leaf3=""),
        SubListB = list(leafA = "", leafB = "")
      ),
      root3 = list(
        SubListA = list(leaf1 = "", leaf2 = "", leaf3=""),
        SubListB = list(leafA = "", leafB = "")
      )
    )
  })
  expect_is(res, "function")
})

test_that("Rlist2json", {  
  li <- list(abc = 123, def = 456)
  res <- shinyTree:::Rlist2json(li)
  expect_is(res, "character")
  expect_true(jsonlite::validate(res))
  expect_length(res, 1)
  
  ## example 01 #################
  li <- list(
    root1 = "",
    root2 = list(
      SubListA = list(leaf1 = "", leaf2 = "", leaf3=""),
      SubListB = list(leafA = "", leafB = "")
    ),
    root3 = list(
      SubListA = list(leaf1 = "", leaf2 = "", leaf3=""),
      SubListB = list(leafA = "", leafB = "")
    )
  )
  res <- shinyTree:::Rlist2json(li)
  expect_is(res, "character")
  expect_true(jsonlite::validate(res))
  expect_length(res, 1)
})

test_that("fixIconName", {
  expect_true(is.null(shinyTree:::fixIconName(NULL)))
  
  expect_true(shinyTree:::fixIconName("/images/ball.jpg") == "/images/ball.jpg") 
  
  g1 <- shinyTree:::fixIconName("glyphicon glyphicon-file")
  g2 <- shinyTree:::fixIconName("glyphicon-file")
  expect_true(g1 == "glyphicon glyphicon-file")
  expect_identical(g1, g2)
  
  f1 <- shinyTree:::fixIconName("fa fa-file")
  f2 <- shinyTree:::fixIconName("fa-file")
  f3 <- shinyTree:::fixIconName("file")  
  expect_true(f1 == "fa fa-file")
  expect_identical(f1, f2)
  expect_identical(f1, f3)
})

## Find parts of shinyTree()'s output by what they are rather than by
## position, which changes whenever a dependency or script is added.
tree_div <- function(res) {
  Filter(function(x) inherits(x, "shiny.tag") && identical(x$attribs$class, "shiny-tree"), res)[[1]]
}
inline_scripts <- function(res) {
  html <- as.character(htmltools::renderTags(res)$html)
  regmatches(html, gregexpr("<script[^>]*>.*?</script>", html))[[1]]
}

test_that("shinyTree", {
  unlistShinytagList <- function(res, checkbox=F, search=F, dnd=F, 
                                 themeicon=T, themedots=T, themestripes=F,
                                 multiple=T, animation=200, sort=F, unique=F,
                                 wholerow=F, types=F, contextmenu=F, 
                                 options = '{"setState":{},"refresh":{}}', 
                                 theme="default") {
    ## Function defaults are the same as shinyTree defaults
    resattr <- tree_div(res)$attribs
    unlist(list(
      checkbox = resattr[["data-st-checkbox"]] == checkbox,
      search = resattr[["data-st-search"]] == search,
      dnd = resattr[["data-st-dnd"]] == dnd,
      themeicon = resattr[["data-st-theme-icons"]] == themeicon,
      themedots = resattr[["data-st-theme-dots"]] == themedots,
      themestripes = resattr[["data-st-theme-stripes"]] == themestripes,
      multiple = resattr[["data-st-multiple"]] == multiple,
      animation = resattr[["data-st-animation"]] == animation,
      sort = resattr[["data-st-sort"]] == sort,
      unique = resattr[["data-st-unique"]] == unique,
      wholerow = resattr[["data-st-wholerow"]] == wholerow,
      types = resattr[["data-st-types"]] == types,
      contextmenu = resattr[["data-st-contextmenu"]] == contextmenu,
      theme = resattr[["data-st-theme"]] == theme,
      options = as.character(resattr[["options"]]) == options,
      options_valid = jsonlite::validate(resattr[["options"]])
    ))
  }


  ## Default shinyTree ############
  res <- shinyTree("tree")
  expect_is(res, "shiny.tag.list")
  expect_true(all(unlistShinytagList(res)))
  
  ## Custom shinyTree with search = TRUE ############
  res <- shinyTree("tree", search = TRUE)
  expect_is(res, "shiny.tag.list")
  expect_true(all(unlistShinytagList(res, search = TRUE)))
  
  ## Custom shinyTree with no animation ############
  res <- shinyTree("tree", animation = FALSE)
  expect_is(res, "shiny.tag.list")
  expect_true(all(unlistShinytagList(res, animation = "false")))
  
  ## Custom shinyTree ############
  res <- shinyTree("tree", checkbox = T, search = "searchid", searchtime = 1000, dragAndDrop = T, 
                   theme = "default", themeIcons = T, themeDots = T, sort = T, unique = T, 
                   wholerow = T, stripes = T, multiple = T, animation = T)
  expect_is(res, "shiny.tag.list")
  expect_true(any(grepl("shinyTree.initSearch('tree','searchid', 1000);",
                        inline_scripts(res), fixed = TRUE)))
  expect_true(all(
    unlistShinytagList(res, checkbox = T, search = T, dnd = T, themeicon = T, 
                       themedots = T, themestripes = T, animation = T, sort = T,
                       unique = T, wholerow = T)))
  
  ## Custom shinyTree with longer animation time ############
  res <- shinyTree("tree", animation = 1000)
  expect_true(all(unlistShinytagList(res, animation = 1000)))
  
  ## Custom shinyTree with types ############
  typs <- "{
          '#': { 'max_children' : 2, 'max_depth' : 4, 'valid_children' : ['root'] },
          'root' : { 'icon' : 'fa fa-signal', 'valid_children' : ['file'] },
          'default' : { 'valid_children' : ['default','file'] },
          'file' : { 'icon' : 'glyphicon glyphicon-file', 'valid_children' : [] }
        }"
  res <- shinyTree("tree", types = typs)
  expect_true(all(unlistShinytagList(res, types = TRUE)))
  expect_true(any(grepl(paste("tree_sttypes =", typs), inline_scripts(res), fixed = TRUE)))
  
  ## Custom shinyTree with different themes ############
  res <- shinyTree("tree", theme = "default-dark")
  # print(unlistShinytagList(res, theme = "default-dark"))
  expect_true(all(unlistShinytagList(res, theme = "default-dark")))
  
  res <- shinyTree("tree", theme = "proton")
  # print(unlistShinytagList(res, theme = "proton"))
  expect_true(all(unlistShinytagList(res, theme = "proton")))
  
  ## ERROR: wrong theme ############
  expect_error(shinyTree("tree", theme = "nonexisting"))
  
  ## WARNING: Checkbox and Contextmenu dont work properly together  ############
  expect_warning(shinyTree("tree", checkbox = T, contextmenu = T))
})

test_that("get_selected_names", {
  ## No Selection ##############
  tree <- list(
    root1 = structure("123"),
    root2 = list(
      SubListA = list(leaf1 = "", leaf2 = "", leaf3=""),
      SubListB = list(leafA = "", leafB = "")
    )
  )
  sel <- get_selected(tree)
  expect_is(sel, "list")
  expect_length(sel, 0)
  
  ## 1 Selection ( root1 ) ##############
  tree <- list(
    root1 = structure("123", stselected = T),
    root2 = list(
      SubListA = list(leaf1 = "", leaf2 = "", leaf3=""),
      SubListB = list(leafA = "", leafB = "")
    )
  )
  sel <- get_selected(tree)
  expect_is(sel, "list")
  expect_length(sel, 1)
  expect_true(sel[[1]][1] == "root1")
  expect_true(length(attr(sel[[1]], "ancestry")) == 0)
  expect_true(attr(sel[[1]], "stselected"))

  ## 1 Selection ( leaf1 ) ##############
  tree <- list(
    root1 = structure("123"),
    root2 = list(
      SubListA = list(leaf1 = structure("", stselected=TRUE), leaf2 = "", leaf3=""),
      SubListB = list(leafA = "", leafB = "")
    )
  )
  sel <- get_selected(tree)
  expect_is(sel, "list")
  expect_length(sel, 1)
  expect_true(sel[[1]][1] == "leaf1")
  expect_true(all(attr(sel[[1]], "ancestry") == c("root2", "SubListA")))
  expect_true(attr(sel[[1]], "stselected"))
  
  ## 2 Selections ( leaf1 / leafB ) ##############
  tree <- list(
    root1 = structure("123"),
    root2 = list(
      SubListA = list(leaf1 = structure("", stselected=TRUE), leaf2 = "", leaf3=""),
      SubListB = list(leafA = structure("", stselected=TRUE), leafB = "")
    )
  )
  sel <- get_selected(tree)
  expect_is(sel, "list")
  expect_length(sel, 2)
  expect_true(sel[[1]][1] == "leaf1")
  expect_true(all(attr(sel[[1]], "ancestry") == c("root2", "SubListA")))
  expect_true(attr(sel[[1]], "stselected"))
  expect_true(sel[[2]][1] == "leafA")
  expect_true(all(attr(sel[[2]], "ancestry") == c("root2", "SubListB")))
  expect_true(attr(sel[[2]], "stselected"))
})
test_that("get_selected_slices", {
  ## No Selection ##############
  tree <- list(
    root1 = structure("123"),
    root2 = list(
      SubListA = list(leaf1 = "", leaf2 = "", leaf3=""),
      SubListB = list(leafA = "", leafB = "")
    )
  )
  sel <- get_selected(tree, "slices")
  expect_is(sel, "list")
  expect_length(sel, 0)
  
  ## 1 Selection ( root1 ) ##############
  tree <- list(
    root1 = structure("123", stselected = T),
    root2 = list(
      SubListA = list(leaf1 = "", leaf2 = "", leaf3=""),
      SubListB = list(leafA = "", leafB = "")
    )
  )
  sel <- get_selected(tree, "slices")
  expect_is(sel, "list")
  expect_length(sel, 1)
  expect_true(names(sel[[1]]) == "root1")
  
  ## 1 Selection ( leaf1 ) ##############
  tree <- list(
    root1 = structure("123"),
    root2 = list(
      SubListA = list(leaf1 = structure("", stselected=TRUE), leaf2 = "", leaf3=""),
      SubListB = list(leafA = "", leafB = "")
    )
  )
  sel <- get_selected(tree, "slices")
  expect_is(sel, "list")
  expect_length(sel, 1)
  expect_true(names(sel[[1]]) == "root2")
  expect_true(names(sel[[1]][[1]]) == "SubListA")
  expect_true(names(sel[[1]][[1]][[1]]) == "leaf1")
  
  ## 2 Selections ( leaf1 / leafB ) ##############
  tree <- list(
    root1 = structure("123"),
    root2 = list(
      SubListA = list(leaf1 = structure("", stselected=TRUE), leaf2 = "", leaf3=""),
      SubListB = list(leafA = structure("", stselected=TRUE), leafB = "")
    )
  )
  sel <- get_selected(tree, "slices")
  expect_is(sel, "list")
  expect_length(sel, 2)
  expect_true(names(sel[[1]]) == "root2")
  expect_true(names(sel[[1]][[1]]) == "SubListA")
  expect_true(names(sel[[1]][[1]][[1]]) == "leaf1")
  expect_true(names(sel[[2]]) == "root2")
  expect_true(names(sel[[2]][[1]]) == "SubListB")
  expect_true(names(sel[[2]][[1]][[1]]) == "leafA")
})
test_that("get_selected_classid", {
  ## No Selection ##############
  tree <- list(
    root1 = structure("123"),
    root2 = list(
      SubListA = list(leaf1 = "", leaf2 = "", leaf3=""),
      SubListB = list(leafA = "", leafB = "")
    )
  )
  sel <- get_selected(tree, "classid")
  expect_is(sel, "list")
  expect_length(sel, 0)
  
  ## 1 Selection ( root1 ) ##############
  tree <- list(
    root1 = structure("123", stselected = T),
    root2 = list(
      SubListA = list(leaf1 = "", leaf2 = "", leaf3=""),
      SubListB = list(leafA = "", leafB = "")
    )
  )
  sel <- get_selected(tree, "classid")
  expect_is(sel, "list")
  expect_length(sel, 1)
  expect_true(sel[[1]] == "root1")
  
  ## 1 Selection ( leaf1 ) ##############
  tree <- list(
    root1 = structure("123"),
    root2 = list(
      SubListA = list(leaf1 = structure("", stselected=TRUE), leaf2 = "", leaf3=""),
      SubListB = list(leafA = "", leafB = "")
    )
  )
  sel <- get_selected(tree, "classid")
  expect_is(sel, "list")
  expect_length(sel, 1)
  expect_true(sel[[1]] == "leaf1")
  
  ## 2 Selections ( leaf1 / leafB ) ##############
  tree <- list(
    root1 = structure("123"),
    root2 = list(
      SubListA = list(leaf1 = structure("", stselected=TRUE), leaf2 = "", leaf3=""),
      SubListB = list(leafA = structure("", stselected=TRUE), leafB = "")
    )
  )
  sel <- get_selected(tree, "classid")
  expect_is(sel, "list")
  expect_length(sel, 2)
  expect_true(sel[[1]] == "leaf1")
  expect_true(sel[[2]] == "leafA")
})

test_that("treeToJSON prunes the root by default and encodes children", {
  skip_if_not_installed("data.tree")
  tree <- make_data_tree()

  js <- treeToJSON(tree)
  expect_true(jsonlite::validate(js))

  parsed <- jsonlite::fromJSON(js, simplifyVector = FALSE)
  expect_length(parsed, 2)
  texts <- unlist(lapply(parsed, function(x) x$text))
  expect_setequal(texts, c("A", "B"))

  a_node <- parsed[[which(texts == "A")]]
  expect_equal(a_node$children[[1]]$text, "A1")
})

test_that("treeToJSON keeps the root node when keepRoot = TRUE", {
  skip_if_not_installed("data.tree")
  tree <- make_data_tree()

  js <- treeToJSON(tree, keepRoot = TRUE)
  parsed <- jsonlite::fromJSON(js, simplifyVector = FALSE)
  expect_null(parsed$text)
  expect_length(parsed$children, 2)
})

test_that("treeToJSON topLevelSlots controls which fields stay out of data", {
  skip_if_not_installed("data.tree")
  tree <- make_data_tree()
  tree$children$A$sticon <- "fa-file"

  js_default <- treeToJSON(tree, topLevelSlots = "default")
  parsed_default <- jsonlite::fromJSON(js_default, simplifyVector = FALSE)
  a_default <- parsed_default[[which(sapply(parsed_default, function(x) x$text) == "A")]]
  expect_equal(a_default$data$sticon, "fa-file")

  js_all <- treeToJSON(tree, topLevelSlots = "all")
  parsed_all <- jsonlite::fromJSON(js_all, simplifyVector = FALSE)
  a_all <- parsed_all[[which(sapply(parsed_all, function(x) x$text) == "A")]]
  expect_null(a_all$data)

  js_explicit <- treeToJSON(tree, topLevelSlots = c("id", "text", "sticon"))
  parsed_explicit <- jsonlite::fromJSON(js_explicit, simplifyVector = FALSE)
  a_explicit <- parsed_explicit[[which(sapply(parsed_explicit, function(x) x$text) == "A")]]
  expect_equal(a_explicit$sticon, "fa-file")
  expect_null(a_explicit$data)
})

test_that("treeToJSON createNewId = FALSE keeps valid unique ids", {
  skip_if_not_installed("data.tree")
  tree <- data.tree::Node$new("myroot")
  a <- tree$AddChild("A")
  a$id <- "ida"
  b <- tree$AddChild("B")
  b$id <- "idb"

  js <- treeToJSON(tree, createNewId = FALSE)
  parsed <- jsonlite::fromJSON(js, simplifyVector = FALSE)
  ids <- unlist(lapply(parsed, function(x) x$id))
  expect_setequal(ids, c("ida", "idb"))
})

test_that("treeToJSON createNewId = FALSE regenerates duplicated/NA ids with a warning", {
  skip_if_not_installed("data.tree")
  tree <- make_data_tree()
  tree$children$A$id <- "dup"
  tree$children$B$id <- "dup"

  expect_warning(js <- treeToJSON(tree, createNewId = FALSE), "invalid")
  parsed <- jsonlite::fromJSON(js, simplifyVector = FALSE)
  ids <- unlist(lapply(parsed, function(x) x$id))
  expect_false(any(duplicated(ids)))
})

test_that("treeToJSON createNewId = TRUE stores a pre-existing id in id.orig with a warning", {
  skip_if_not_installed("data.tree")
  tree <- make_data_tree()
  tree$children$A$id <- "origid"

  expect_warning(js <- treeToJSON(tree, createNewId = TRUE), "id.orig")
  parsed <- jsonlite::fromJSON(js, simplifyVector = FALSE)
  a_node <- parsed[[which(sapply(parsed, function(x) x$text) == "A")]]
  expect_equal(a_node$data$id.orig, "origid")
})

test_that("treeToJSON drops unsupported slot types with a warning", {
  skip_if_not_installed("data.tree")
  tree <- make_data_tree()
  tree$children$B$myfunc <- function() 1

  expect_warning(js <- treeToJSON(tree), "unsupported slot")
  parsed <- jsonlite::fromJSON(js, simplifyVector = FALSE)
  b_node <- parsed[[which(sapply(parsed, function(x) x$text) == "B")]]
  expect_null(b_node$data$myfunc)
})

test_that("treeToJSON warns about invalid state names but keeps the valid ones", {
  skip_if_not_installed("data.tree")
  tree <- make_data_tree()
  tree$children$A$state <- list(selected = TRUE, bogus = TRUE)

  expect_warning(js <- treeToJSON(tree), "invalid state")
  parsed <- jsonlite::fromJSON(js, simplifyVector = FALSE)
  a_node <- parsed[[which(sapply(parsed, function(x) x$text) == "A")]]
  expect_true(a_node$state$selected)
  expect_false(a_node$state$disabled)
})

test_that("treeToJSON errors when data.tree is not installed", {
  skip_if_not_installed("data.tree")
  skip_if_not_installed("mockery")
  tree <- make_data_tree()

  mockery::stub(treeToJSON, "requireNamespace", FALSE)
  expect_error(treeToJSON(tree), "data.tree")
})

test_that("shinyTree declares its scripts and styles as dependencies (#18)", {
  ## Written into <head> they were dropped by R Markdown documents with
  ## runtime: shiny, leaving the tree empty.
  deps <- htmltools::resolveDependencies(
    htmltools::renderTags(htmltools::tagList(
      shinyTree("a"), shinyTree("b", theme = "proton"), shinyTree("c")
    ))$dependencies
  )
  names <- vapply(deps, function(d) d$name, "")
  expect_setequal(names, c("jstree", "jstree-theme-default", "jstree-theme-proton",
                           "shinyTree", "font-awesome-shinyTree"))
  ## Each is included once however many trees use it.
  expect_false(anyDuplicated(names) > 0)

  files <- unlist(lapply(deps, function(d) {
    if (is.null(d$src$file)) return(character())
    file.path(d$src$file, c(unlist(d$script), unlist(d$stylesheet)))
  }))
  expect_true(all(file.exists(files)))
  ## Nothing is written into <head> directly any more.
  expect_identical(as.character(htmltools::renderTags(shinyTree("a"))$head), "")
})
