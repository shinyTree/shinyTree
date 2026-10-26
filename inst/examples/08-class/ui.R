library(shiny)
library(shinyTree)

#' Define UI for application that demonstrates a simple Tree editor
#' @author Jeff Allen \email{jeff@@trestletech.com}
shinyUI(
  pageWithSidebar(
    headerPanel("shinyTree with Custom CSS Class"),
    
    sidebarPanel(      
      helpText(HTML("The first two roots are coloured by their jsTree <code>sttype</code>.
                    Under <em>tasks</em>, each node gets CSS classes from
                    <code>stcssclass</code>, styled by the CSS in this page:
                    <code>done</code> is struck through, <code>important</code> is bold,
                    and a node can have both.
                    <hr />Created using <a href = \"https://github.com/shinyTree/shinyTree\">shinyTree</a>."))
    ),
    mainPanel(
      tags$style(HTML("
        .shiny-tree li.done > a { text-decoration: line-through; color: #888; }
        .shiny-tree li.important > a { font-weight: bold; }
      ")),
      shinyTree("tree",
                types = "{ 'red-node': {'a_attr' : { 'style' : 'color:red' }}, 
                'blue-node': {'a_attr' : { 'style' : 'color:blue' }} }" )
    )
  ))