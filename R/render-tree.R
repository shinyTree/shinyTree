#' Render a ShinyTree
#'
#' Should return a list from the given expression which will be converted into a
#' \code{\link{shinyTree}}.
#'
#' @param expr The expression to be evaluated which should produce a list.
#' @param env The environment in which \code{expr} should be evaluated.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#' is useful if you want to save an expression in a variable.
#' @section Note:
#' A tree's contents are pushed to the client rather than returned as an output
#' value, so Shiny has nothing cached to replay if the element is removed from
#' the page and later added back -- inside a \code{\link[shiny]{modalDialog}}
#' that is dismissed and reopened, for example, or a conditional panel. The
#' client reports whenever it binds an empty tree and the contents are resent,
#' so such a tree repopulates without the app having to re-trigger the render
#' itself.
#' @seealso \code{\link{shinyTree}}
#' @export
renderTree <- function(expr, env = parent.frame(), quoted = FALSE){
  func <- shiny::exprToFunction(expr, env, quoted)
  ## Guards against registering the rebind observer more than once for a given
  ## output, since the render function runs again whenever expr invalidates.
  observer_registered <- FALSE
  return(function(shinysession, name, ...) {
    tree <- func()
    updateTree(shinysession, name, tree)

    if (!observer_registered && !is.null(shinysession$input)) {
      observer_registered <<- TRUE
      registerRebindObserver(shinysession, name, func)
    }
    NULL
  })
}

#' Resend a tree when the client reports binding an empty one
#'
#' The client sets \code{<treeId>_rebind} whenever it binds a \code{shinyTree}
#' element that has no nodes. That happens when the element is added to the page
#' again after being removed, at which point Shiny will not re-run the render
#' function because the output is still considered valid. Resending the contents
#' repopulates it.
#'
#' @param session the Shiny session
#' @param treeId the output id of the tree
#' @param func function returning the tree contents
#' @noRd
registerRebindObserver <- function(session, treeId, func) {
  rebindId <- paste0(treeId, "_rebind")
  shiny::observeEvent(session$input[[rebindId]],
                      {
                        updateTree(session, treeId, func())
                      },
                      ignoreInit = TRUE,
                      ignoreNULL = TRUE,
                      domain = session)
  invisible()
}

#' Render an empty ShinyTree
#' 
#' Renders a tree with no defined nodes.
#'
#' @seealso \code{\link{shinyTree}}
#' @export
renderEmptyTree <- function() {
  return(function(shinysession, name) {
  })
}
