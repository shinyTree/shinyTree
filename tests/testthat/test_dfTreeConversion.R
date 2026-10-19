
test_that("dfrapply errors on list elements that are neither data.frame nor list", {
  expect_error(
    shinyTree:::dfrapply(list("not a data frame or list"), function(x) x),
    "data frame or another list"
  )
})

test_that("data tree conversion works back and forward",{
      myData <- mtcars
      myDataChar <- data.frame(
          lapply(myData, as.character),
          stringsAsFactors=FALSE)
      
      hierarchy <- colnames(myData)
      
      tree <- dfToTree(myData, hierarchy)
      df <- treeToDf(tree, hierarchy)[, hierarchy]
      
      expect_equal(nrow(unique(myData)), nrow(df))
      expect_equal(sort(colnames(myData)), sort(colnames(df)))
    })

test_that("depth of an atomic (non-list) value is 0", {
  expect_equal(shinyTree:::depth("leaf"), 0L)
  expect_equal(shinyTree:::depth(5), 0L)
  expect_equal(shinyTree:::depth(NULL), 0L)
})

test_that("depth of a nested list reflects the number of levels", {
  tree <- list(a = list(b = list(c = "")))
  expect_equal(shinyTree:::depth(tree), 3L)
})

test_that("treeToDf falls back to numeric column names when hierarchy length does not match depth", {
  tree <- list(
    "folder1" = list("file1" = "", "file2" = ""),
    "folder2" = list("file3" = "")
  )

  df_mismatched <- treeToDf(tree, hierarchy = c("onlyonename"))
  expect_equal(names(df_mismatched), c("1", "2"))

  df_null <- treeToDf(tree, hierarchy = NULL)
  expect_equal(names(df_null), c("1", "2"))

  df_matched <- treeToDf(tree, hierarchy = c("folder", "file"))
  expect_equal(names(df_matched), c("folder", "file"))
})

test_that("An unbalanced tree conversion works back and forth",{
      myList <- list(
          "A" = list(
              "AA" = list(
                  "AAA"="",
                  "AAB"=""
              ),
              "AB"=""),
          "B" = list(
              "BA"="",
              "BB"="")
      )
      df <- treeToDf(myList)
      newList <- dfToTree(df)
      expect_equal(myList,newList)
      
    })
