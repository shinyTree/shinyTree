library(shinyTree)

shinyUI(
	fluidPage(
		sidebarLayout(
			sidebarPanel(
				selectInput("idTree",
							label = "Select a tree",
							choices = list("",
							               "A" = "A",
							               "A_closed" = "A_closed",
							               "B" = "B", 
							               "List" ="List"),
							selected = NULL)
			),
			mainPanel(
			  h4("Tree Update"),
			  "The tree is initialized, but not populated until a choices is selected in the dropdown menu.",
			  shinyTree("tree"),
			  hr(),
				"Currently selected:",
				verbatimTextOutput("idSelected")#,
			)
		)
	)
)
