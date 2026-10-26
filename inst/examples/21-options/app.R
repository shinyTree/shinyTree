library(shiny)
library(shinyTree)

## The options shinyTree.refresh and shinyTree.setState decide whether a
## refresh or set_state started in the browser -- by the app's own JavaScript,
## say -- is reported in input$tree. They do not affect trees sent by the
## server: a tree from renderTree() or updateTree() is always reported once it
## has loaded, and the refresh that loads it is not reported again, which
## would loop in apps whose observers call updateTree().
##
## Both must be set before the UI is built, as they are written into the page.
## This app builds two trees, one with each setting, and refreshes both from
## JavaScript.

tree <- list(a = structure(list(a1 = "", a2 = ""), stopened = TRUE), b = "")

ui <- fluidPage(
  titlePanel("shinyTree options"),
  sidebarLayout(
    sidebarPanel(
      actionButton("js_refresh", "Refresh both trees from JavaScript"),
      actionButton("server_update", "updateTree() both trees"),
      hr(),
      helpText(HTML(
        "<code>options(shinyTree.refresh = TRUE)</code> reports refreshes started",
        "in the browser. Select a node in each tree, then refresh from",
        "JavaScript, which clears the selection: only the left tree reports it,",
        "so the right tree's <code>input$silent</code> still holds the old",
        "selection.<br><br>",
        "A tree sent with <code>updateTree()</code> is reported once in both,",
        "whatever the options, if it differs from what <code>input$tree</code>",
        "holds -- here it clears the selection."
      )),
      tags$script(HTML(
        "$(document).on('click', '#js_refresh', function() {
           ['reported', 'silent'].forEach(function(id) {
             // forget_state = true: refresh(true) alone restores the
             // selection as it reloads, a changed event every tree reports,
             // so the difference the option makes would not show.
             $.jstree.reference(document.getElementById(id)).refresh(true, true);
           });
         });"
      ))
    ),
    mainPanel(
      fluidRow(
        column(6,
          h4("shinyTree.refresh = TRUE"),
          local({
            old <- options(shinyTree.refresh = TRUE, shinyTree.setState = TRUE)
            on.exit(options(old))
            shinyTree("reported")
          }),
          verbatimTextOutput("reported_count")
        ),
        column(6,
          h4("shinyTree.refresh = FALSE (default)"),
          local({
            old <- options(shinyTree.refresh = FALSE, shinyTree.setState = FALSE)
            on.exit(options(old))
            shinyTree("silent")
          }),
          verbatimTextOutput("silent_count")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  output$reported <- renderTree(tree)
  output$silent <- renderTree(tree)

  counts <- reactiveValues(reported = 0, silent = 0)
  observeEvent(input$reported, counts$reported <- counts$reported + 1)
  observeEvent(input$silent, counts$silent <- counts$silent + 1)
  output$reported_count <- renderText(sprintf(
    "input$reported updates: %d\nselected: %s", counts$reported,
    paste(unlist(get_selected(input$reported)), collapse = ", ")))
  output$silent_count <- renderText(sprintf(
    "input$silent updates: %d\nselected: %s", counts$silent,
    paste(unlist(get_selected(input$silent)), collapse = ", ")))

  observeEvent(input$server_update, {
    updateTree(session, "reported", tree)
    updateTree(session, "silent", tree)
  })
}

shinyApp(ui, server)
