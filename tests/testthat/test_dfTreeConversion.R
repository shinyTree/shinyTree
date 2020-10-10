
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

test_that("nodes of nested named list can be stacked to data.frame",{
      myList <- list(
          "A" = list(
              "AA" = list(
                  "AAA" = "",
                  "AAB" = ""
              ),
              "AB" = ""),
          "B" = list(
              "BA" = "",
              "BB" = "")
      )
      newList <- nodesToDf(myList, c("1","2","3"))
      expect_equal(names(newList), c("A", "B"))
      expect_equal(colnames(newList$A), c("2", "3"))
      expect_equal(colnames(newList$B), "2")
      expect_true(nrow(newList$A) == 3)
      expect_true(nrow(newList$B) == 2)
    })

test_that("named list of data.frames can be stacked to single data.frame",{
      myList = list(
          "A" = data.frame(x = "AX", y = "AY"),
          "B" = data.frame(x = "BX", y = "BY", z = "BZ")
      )
      df <- stackList(myList, "name")
      expect_true(all(c("x", "y", "z", "name") %in% colnames(df)))
      expect_true(inherits(df, "data.frame"))
      expect_true(nrow(df) == 2)
    })

test_that("named list of empty entries can be stacked to data.frame",{
      myList = list(
          "A" = "",
          "B" = ""
      )
      df <- stackList(myList, "name")
      expect_true(colnames(df) == "name")
      expect_true(inherits(df, "data.frame"))
      expect_true(nrow(df) == 2)
      expect_true(ncol(df) == 1)
    })
