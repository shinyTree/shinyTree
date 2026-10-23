library(shiny)
library(shinyTree)

#' Define server logic for an application browsing an ontology
shinyServer(function(input, output, session) {
  ## Allow uploads of large ontologies (the Gene Ontology is about 32 MB).
  options(shiny.maxRequestSize = 200 * 1024^2)

  ontology <- reactive({
    if (identical(input$source, "upload")) {
      req(input$file)
      ## The upload keeps its name in input$file$name; the saved copy may not.
      ext <- tolower(tools::file_ext(input$file$name))
      validate(need(ext %in% c("obo", "owl"), "Choose an .obo or .owl file."))
      tryCatch(ontologyToTree(input$file$datapath, format = ext),
               error = function(e) validate(need(FALSE, conditionMessage(e))))
    } else {
      ontologyToTree("aeo.obo")
    }
  })

  output$tree <- renderTree(lazy = TRUE, ontology())

  output$summary <- renderText({
    terms <- attr(ontology(), "ontology")
    sprintf("%s terms, %s obsolete", format(nrow(terms), big.mark = ","),
            format(sum(terms$obsolete), big.mark = ","))
  })

  output$selected <- renderPrint({
    tree <- req(input$tree)
    for (n in get_selected(tree)) {
      cat(attr(n, "stid"), " ", n, "\n", sep = "")
    }
  })
})
