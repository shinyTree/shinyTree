context("test_tree")
library(shiny)
library(jsonlite)


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

test_that("shinyTree", {
  unlistShinytagList <- function(res, checkbox=F, search=F, dnd=F, 
                                 themeicon=T, themedots=T, themestripes=F,
                                 multiple=T, animation=200, sort=F, unique=F,
                                 wholerow=F, types=F, contextmenu=F, 
                                 options = '{"setState":{},"refresh":{}}', 
                                 theme="default") {
    ## Function defaults are the same as shinyTree defaults
    resattr <- res[[3]]$attribs
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
  expect_identical(res[[2]]$children[[2]]$children[[1]],
                   HTML("shinyTree.initSearch('tree','searchid', 1000);"))
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
  expect_identical(res[[1]]$children[[6]]$children[[1]],
                   HTML(paste("tree_sttypes =", typs)))
  
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
