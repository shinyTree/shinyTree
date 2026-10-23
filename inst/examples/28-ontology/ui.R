library(shiny)
library(shinyTree)

#' Define UI for an application browsing an ontology
shinyUI(
  pageWithSidebar(
    headerPanel("shinyTree with an ontology"),

    sidebarPanel(
      radioButtons("source", NULL,
                   c("Anatomical Entity Ontology (AEO)" = "aeo",
                     "Upload a file" = "upload")),
      conditionalPanel(
        "input.source == 'upload'",
        fileInput("file", NULL, accept = c(".obo", ".owl"),
                  placeholder = "an .obo or .owl file")
      ),
      helpText(HTML("<code>ontologyToTree()</code> reads an OBO or OWL ontology
                    and <code>renderTree(lazy = TRUE)</code> shows it a level at a
                    time, so even the Gene Ontology's 48,000 terms load at once.
                    <br><br>Hover over a term for its identifier and definition. A
                    term with several parents appears under each of them; obsolete
                    terms are greyed out under <em>obsolete</em>. The search box
                    finds a term by name or identifier anywhere in the ontology.
                    <br><br>The AEO is the example from issue #33; see this example's
                    README for its source and licence.
                    <hr>Created using <a href = \"http://github.com/shinyTree/shinyTree\">shinyTree</a>."))
    ),
    mainPanel(
      textOutput("summary"),
      shinyTree("tree", search = TRUE, searchplaceholder = "Search by name or id"),
      hr(),
      "Selected:",
      verbatimTextOutput("selected")
    )
  )
)
