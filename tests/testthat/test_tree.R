context("test_tree")
library(shiny)

# Wrap a tag with an data-jstree attribute
jst <- function(t, attrib="{}"){
  return(tagAppendAttributes(t, `data-jstree`=attrib))
}

test_that("named list works", {  
  # li <- list(abc = 123, def = 456)
  # expect_equal(shinyTree:::listToTags(li),
  #              jst(tags$ul(
  #                jst(tags$li("abc")), 
  #                jst(tags$li("def"))
  #              )
  #              ))
})