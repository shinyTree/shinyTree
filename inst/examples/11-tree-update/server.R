# server.R
library(shiny)
library(shinyTree)

jsonTree<- function(idTree){
	switch(idTree,
    "A" = '[{"id":"node_2","text":"Root node with options","state":{"opened":true,"selected":true},"children":[{"text":"Child 1"},"Child 2"]}]',
    "A_closed" = '[{"id":"node_2","text":"Root node with options","state":{"opened":false,"selected":false},"children":[{"text":"Child 1"},"Child 2"]}]',
		"B" = '[{"id":"ajson1","parent":"#","text":"Simplerootnode","li_attr":{"class":"project","stid":"project-1"}},{"id":"ajson2","parent":"#","text":"Rootnode2"},{"id":"ajson3","parent":"ajson2","text":"Child1"},{"id":"ajson4","parent":"ajson2","text":"Child2"}]',
		"List" = list(a=list(a1=1,a2=2) , b="b") )
}

shinyServer(function(input, output, session) {
	output$idSelected <- renderPrint({
		tree <- input$tree
		if (is.null(tree)){
			"None"
		} else{
			str(get_selected(input$tree, format = "classid"))
		}
	})
	
	# Tree is initialized without nodes
	output$tree <- renderEmptyTree()
  
	# An observer is used to trigger a tree update with new data.
	observe({
	  updated.tree<-jsonTree(input$idTree)
	  updateTree(session,"tree",updated.tree)
	})
})
