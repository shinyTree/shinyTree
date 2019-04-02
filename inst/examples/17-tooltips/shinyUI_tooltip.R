library(shiny)
library(shinyTree)

js <- {HTML("
$(document).on('shiny:connected', function(event) {
  $('#tree')
      .on('hover_node.jstree', function (e, data) {
        var $node = $('#' + data.node.id);
        var x = $node.position().top,
            y = $node.position().left;
      
        Shiny.setInputValue('hoverednode', data.node.text);
        var tooltipSpan = $('#hoverinfo')[0];
        tooltipSpan.style.opacity = 1;
        tooltipSpan.style.top = x + 'px';
        tooltipSpan.style.left = y + 160 + 'px';
      })
      .on('dehover_node.jstree', function(e, data) {
        var tooltipSpan = $('#hoverinfo')[0];
        tooltipSpan.style.opacity = 0;
      });
});")}

css <- {HTML("
#hoverinfo {
  position: absolute;
  background-color: black;
  color: #fff;
  z-index: 10;
  text-align: center;
  border-radius: 6px;
  padding: 5px;
  font-weight: bold;
  opacity: 0;
}
")}

ui <- {fluidPage(
  tags$head(tags$script(js)),
  tags$head(tags$style(css)),
  shinyTree("tree"),
  uiOutput("hoverinfo", inline=T)
)}

server <- function(input, output, session) {
  output$tree <- renderTree({
    list(
      root1 = structure("123"),
      root2 = list(
        SubListA = list(leaf1 = "", leaf2 = "", leaf3=""),
        SubListB = list(leafA = "", leafB = "")
      ),
      root3 = structure("123"),
      root4 = structure("123"),
      root5 = structure("123"),
      root6 = structure("123")
    )
  })
  
  output$hoverinfo <- renderUI({
    req(input$hoverednode)
    paste("The hovered node text is: ", input$hoverednode)  
  })
}

shinyApp(ui, server)