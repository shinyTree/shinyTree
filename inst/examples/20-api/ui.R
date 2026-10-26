library(shiny)
library(shinyjs)
library(shinyTree)

jsFun <- "
  shinyjs.resetTree = function(id) {
    // Deselect all of the leaves and nodes  
    var tree = $.jstree.reference(document.getElementById(id));
    tree.deselect_all();

    // Close all of the nodes
    tree.close_all();
  };
  "

#' Define custom JS functions to implement jsTree core functionality
#' @author McClelland Legge \email{McClelland.Legge@@gmail.com}
shinyUI(
  pageWithSidebar(
    headerPanel("shinyTree with Core jsTree Function"),
    
    sidebarPanel(      
      helpText(p(""),
               HTML("<hr />The reset button uses the `deselect_all` and `close_all` functions, found in the <a href = \"https://www.jstree.com/api\">API documentation</a>, to deselect all of the selected nodes/leaves and then close all nodes.",
                    "To open or close every node from R, without any JavaScript, see <code>expandTree()</code> and <code>collapseTree()</code>.")),
      actionButton("reset", "Reset Tree")
    ),
    
    mainPanel(
      useShinyjs(),
      extendShinyjs(text = jsFun, functions = "resetTree"),
      shinyTree("tree", checkbox = TRUE)
    )
  ))