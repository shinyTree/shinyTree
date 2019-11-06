context("test_tree_shinytest")
library(shiny)
library(jsonlite)
library(shinytest)
library(testthat)

# open Shiny app and PhantomJS
app <- ShinyDriver$new(test_path("apps/test_selec"))
test_that("interactive_tests", {
  skip_on_cran()
  
  x <- app$getAllValues()
  
  expect_true(x$input$action == 0)
  expect_equal(x$output$sel_names, "[[1]]\n[1] \"root1\"\nattr(,\"ancestry\")\ncharacter(0)\nattr(,\"stselected\")\n[1] TRUE\n")
  expect_equal(x$output$sel_classid, "[[1]]\n[1] \"root1\"\nattr(,\"id\")\n[1] \"1\"\n")
  expect_equal(x$output$sel_slices, "[[1]]\n[[1]]$root1\n[1] 0\n\n")
  
  app$setInputs(
    action = "click"
  )
  x <- app$getAllValues()

  expect_true(x$input$action == 1)
  expect_equal(x$output$sel_names, "[[1]]\n[1] \"leaf1\"\nattr(,\"ancestry\")\n[1] \"root2\"    \"SubListA\"\nattr(,\"stselected\")\n[1] TRUE\n")
  expect_equal(x$output$sel_classid, "[[1]]\n[1] \"leaf1\"\nattr(,\"id\")\n[1] \"4\"\n")
  expect_equal(x$output$sel_slices, "[[1]]\n[[1]]$root2\n[[1]]$root2$SubListA\n[[1]]$root2$SubListA$leaf1\n[1] 0\n\n\n\n")
  
})
app$stop()


# open Shiny app and PhantomJS
app <- ShinyDriver$new(test_path("apps/test_async"))
test_that("interactive_tests_async", {
  skip_on_cran()
  
  x <- app$getAllValues()
  
  expect_true(x$input$action == 0)
  expect_equal(x$output$sel_names, "[[1]]\n[1] \"root1\"\nattr(,\"ancestry\")\ncharacter(0)\nattr(,\"stselected\")\n[1] TRUE\n")
  expect_equal(x$output$sel_classid, "[[1]]\n[1] \"root1\"\nattr(,\"id\")\n[1] \"1\"\n")
  expect_equal(x$output$sel_slices, "[[1]]\n[[1]]$root1\n[1] 0\n\n")
  
  app$setInputs(
    action = "click"
  )
  x <- app$getAllValues()
  
  expect_true(x$input$action == 1)
  expect_equal(x$output$sel_names, "[[1]]\n[1] \"leaf1\"\nattr(,\"ancestry\")\n[1] \"root2\"    \"SubListA\"\nattr(,\"stselected\")\n[1] TRUE\n")
  expect_equal(x$output$sel_classid, "[[1]]\n[1] \"leaf1\"\nattr(,\"id\")\n[1] \"4\"\n")
  expect_equal(x$output$sel_slices, "[[1]]\n[[1]]$root2\n[[1]]$root2$SubListA\n[[1]]$root2$SubListA$leaf1\n[1] 0\n\n\n\n")
})
app$stop()
