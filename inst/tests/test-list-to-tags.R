context("test_list_to_tags")
test_that("named list works", {  
  li <- list(abc = 123, def = 456)
  expect_equal(listToTags(li), 
               tags$ul(
                 tags$li("abc", `data-jstree`="{}"), 
                 tags$li("def", `data-jstree`="{}")
               )
              )
})

test_that("unnamed list works", {
  li <- list()
  li[[1]] <- 123
  li[[2]] <- 456
  expect_equal(listToTags(li), tags$ul(tags$li("123"), tags$li("456")))
})

test_that("opened attributes work", {
  li <- structure(list(a=1, b=2), stopened=TRUE)
  expect_equal(listToTags(li),
    tags$ul(tags$li("a"), tags$li("b"), `data-jstree`="{\"opened\": true}")
  )
})

test_that("nested opened attributes work", {
  li <- list(abc = "hi", def=structure(list(a=1, b=2), stopened=TRUE))
  expect_equal(listToTags(li), 
    tags$ul(tags$li("abc"), tags$li("def", 
      tags$ul(`data-jstree`="{\"opened\": true}", tags$li("a"), tags$li("b"))
    ))
  )
})

test_that("selected attributes work", {
  li <- structure(list(a=1, b=2), stselected=TRUE)
  expect_equal(listToTags(li),
               tags$ul(tags$li("a"), tags$li("b"), `data-jstree`="{\"selected\": true}")
  )
})

test_that("disabled attributes work", {
  li <- structure(list(a=1, b=2), stdisabled=TRUE)
  expect_equal(listToTags(li),
               tags$ul(tags$li("a"), tags$li("b"), `data-jstree`="{\"disabled\": true}")
  )
})

test_that("multiple modifications work", {
  li <- structure(list(a=1, b=2), stopened=TRUE, stselected=TRUE, stdisabled=TRUE)
  expect_equal(listToTags(li),
               tags$ul(tags$li("a"), tags$li("b"), `data-jstree`=
                         "{\"opened\": true,\"selected\": true,\"disabled\": true}")
  )
})