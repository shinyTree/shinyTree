
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
