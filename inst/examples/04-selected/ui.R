library(shiny)
library(shinyTree)

#' Define UI for an application comparing selected and checked nodes
#' @author Jeff Allen \email{jeff@@trestletech.com}
shinyUI(
  pageWithSidebar(
    # Application title
    headerPanel("shinyTree with 'selected' and 'checked' input"),

    sidebarPanel(
      checkboxInput("checkbox", "checkbox", TRUE),
      conditionalPanel(
        "input.checkbox",
        checkboxInput("tie_selection", "tie_selection", TRUE),
        checkboxInput("whole_node", "whole_node", TRUE),
        checkboxInput("three_state", "three_state", TRUE)
      ),
      radioButtons("format", "format", c("names", "slices", "classid"),
                   inline = TRUE),
      hr(),
      uiOutput("explanation"),
      helpText(HTML("<hr>Created using <a href = \"https://github.com/shinyTree/shinyTree\">shinyTree</a>."))
    ),
    mainPanel(
      uiOutput("tree_ui"),
      hr(),
      fluidRow(
        column(6, "get_selected():", verbatimTextOutput("selected")),
        column(6, "get_checked():", verbatimTextOutput("checked"))
      )
    )
  ))
