library(shiny)
library(shinyjs)
library(shinyTree)

jsFun <- "
  shinyjs.resetTree = function(id) {
    // Deselect all of the leaves and nodes  
    $('#' + id).jstree(true).deselect_all();

    // Close all of the nodes
    $('#' + id).jstree(true).close_all();
  };
  "

#' Define custom JS functions to implement jsTree core functionality
#' @author McClelland Legge \email{McClelland.Legge@@gmail.com}
shinyUI(
  pageWithSidebar(
    useShinyjs(),
    extendShinyjs(text = jsFun),
    headerPanel("shinyTree with Core jsTree Function"),
    
    sidebarPanel(      
      helpText(p(""),
               HTML("<hr />The reset button uses the `deselect_all` and `close_all` functions, found in the <a href = \"https://www.jstree.com/api\">API documentation</a>, to deselect all of the selected nodes/leaves and then close all nodes.")),
      actionButton("reset", "Reset Tree")
    ),
    
    mainPanel(
      shinyTree("tree", checkbox = TRUE)
    )
  ))