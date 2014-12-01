context("test_list_to_tags")

library(shiny)

# Wrap a tag with an data-jstree attribute
jst <- function(t, attrib="{}"){
  return (tagAppendAttributes(t, `data-jstree`=attrib))
}

test_that("named list works", {  
  li <- list(abc = 123, def = 456)
  expect_equal(shinyTree:::listToTags(li),
               jst(tags$ul(
                 jst(tags$li("abc")), 
                 jst(tags$li("def"))
               )
              ))
})

test_that("opened attributes work", {
  li <- structure(list(a=1, b=2), stopened=TRUE)
  expect_equal(shinyTree:::listToTags(li),
    jst(tags$ul(
      jst(tags$li("a")),
      jst(tags$li("b"))),
      "{\"opened\": true}")
  )
})

test_that("class attributes work", {
  li <- structure(list(a=1, b=2), stclass="class-a")
  expect_equal(shinyTree:::listToTags(li),
               jst(tags$ul(
                 jst(tags$li("a")), 
                 jst(tags$li("b")),
                 class="class-a"))
  )
})

test_that("nested opened attributes work", {
  li <- list(abc = "hi", def=structure(list(a=1, b=2), stopened=TRUE))
  expect_equal(shinyTree:::listToTags(li),
    jst(tags$ul(
      jst(tags$li("abc")), 
      jst(tags$li("def", 
        jst(tags$ul(
          jst(tags$li("a")), 
          jst(tags$li("b"))
        ), "{\"opened\": true}")
        ), "{\"opened\": true}"))
      )
    )
})

test_that("selected attributes work", {
  li <- structure(list(a=1, b=2), stselected=TRUE)
  expect_equal(shinyTree:::listToTags(li),
               jst(tags$ul(
                 jst(tags$li("a")), 
                 jst(tags$li("b"))), 
                 "{\"selected\": true}")
               
  )
})

test_that("disabled attributes work", {
  li <- structure(list(a=1, b=2), stdisabled=TRUE)
  expect_equal(shinyTree:::listToTags(li),
               jst(tags$ul(
                 jst(tags$li("a")), 
                 jst(tags$li("b"))),
                 "{\"disabled\": true}")
  )
})

test_that("multiple modifications work", {
  li <- structure(list(a=1, b=2), stopened=TRUE, stselected=TRUE, stdisabled=TRUE)
  expect_equal(shinyTree:::listToTags(li),
               jst(tags$ul(
                 jst(tags$li("a")), 
                 jst(tags$li("b"))),
                 "{\"opened\": true,\"selected\": true,\"disabled\": true}")
  )
})