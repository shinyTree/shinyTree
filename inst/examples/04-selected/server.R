library(shiny)
library(shinyTree)

#' Define server logic comparing get_selected() and get_checked() under each
#' combination of the checkbox options
#' @author Jeff Allen \email{jeff@@trestletech.com}
shinyServer(function(input, output, session) {

  # The checkbox options are fixed when the tree is created, so the tree is
  # rebuilt whenever one of them changes.
  output$tree_ui <- renderUI({
    shinyTree("tree123",
              checkbox = input$checkbox,
              tie_selection = input$tie_selection,
              whole_node = input$whole_node,
              three_state = input$three_state)
  })

  output$tree123 <- renderTree({
    # Rebuilding the tree element above needs its contents sent again.
    list(input$checkbox, input$tie_selection, input$whole_node, input$three_state)
    list(
      root1 = structure("123"),
      root2 = structure(
        list(
          SubListA = list(leaf1 = "", leaf2 = "", leaf3 = ""),
          SubListB = list(leafA = "", leafB = "")
        ),
        stopened = TRUE
      )
    )
  })

  output$selected <- renderPrint({
    tree <- req(input$tree123)
    str(get_selected(tree, format = input$format))
  })
  output$checked <- renderPrint({
    tree <- req(input$tree123)
    str(get_checked(tree, format = input$format))
  })

  output$explanation <- renderUI({
    if (!isTRUE(input$checkbox)) {
      return(helpText(HTML(
        "Without checkboxes, clicking a node selects it. Hold Ctrl or Shift to
        select several. <code>get_selected()</code> reports them;
        <code>get_checked()</code> has nothing to report."
      )))
    }
    tie <- if (isTRUE(input$tie_selection)) {
      "<b>tie_selection = TRUE</b>: checked and selected are the same thing.
      Ticking a box selects the node, selecting a node ticks its box, and
      <code>get_checked()</code> and <code>get_selected()</code> always agree."
    } else {
      "<b>tie_selection = FALSE</b>: checked and selected are independent. A
      node can be checked without being selected, which suits trees where
      selecting opens an item and checking marks it for an action."
    }
    whole <- if (isTRUE(input$whole_node) && isTRUE(input$tie_selection)) {
      "<b>whole_node = TRUE</b>: clicking anywhere on a node toggles it, as if
      the box were clicked, so nodes add to the selection one click at a time."
    } else if (isTRUE(input$whole_node)) {
      "<b>whole_node = TRUE</b>: clicking anywhere on a node toggles its box.
      Since checking is untied, nothing can be selected with the mouse at all
      -- <code>get_selected()</code> stays empty. Turn <code>whole_node</code>
      off to select by clicking the label."
    } else if (isTRUE(input$tie_selection)) {
      "<b>whole_node = FALSE</b>: clicking the box toggles it and adds to the
      selection; clicking the label selects that node alone, replacing the
      selection."
    } else {
      "<b>whole_node = FALSE</b>: clicking the box checks the node; clicking
      the label selects it."
    }
    three <- if (isTRUE(input$three_state)) {
      "<b>three_state = TRUE</b>: checking a parent checks all of its children,
      and a parent is checked only while all of its children are. With
      <code>tie_selection = TRUE</code> this cascades to the selection too."
    } else {
      "<b>three_state = FALSE</b>: each box is independent of its parent and
      children."
    }
    helpText(HTML(paste(tie, whole, three, sep = "<br><br>")))
  })
})
