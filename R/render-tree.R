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
#' @section Node attributes:
#' Each node is an element of the list, named by its label. Attributes set on
#' it with \code{\link{structure}} or \code{attr()} control how it is shown:
#' \describe{
#'   \item{\code{stopened}}{\code{TRUE} to show the node expanded.}
#'   \item{\code{stselected}}{\code{TRUE} to select it.}
#'   \item{\code{stchecked}}{\code{TRUE} to tick its checkbox when
#'     \code{tie_selection = FALSE}. With the default
#'     \code{tie_selection = TRUE} a ticked box is a selection, so use
#'     \code{stselected} instead.}
#'   \item{\code{stdisabled}}{\code{TRUE} to disable it.}
#'   \item{\code{sticon}}{an icon: a Font Awesome or glyphicon name such as
#'     \code{"fa fa-file"}, or the URL of an image.}
#'   \item{\code{sttype}}{the node's type, for the \code{types} argument of
#'     \code{\link{shinyTree}}.}
#'   \item{\code{sttooltip}}{text shown when the pointer rests on the node's
#'     label. It is shown as plain text, not HTML.}
#'   \item{\code{stcssclass}}{one or more CSS classes for the node, as a
#'     character vector or a space-separated string, so the app's own CSS can
#'     style it. They go on the node's \code{<li>}, so target its label with
#'     \code{li.myclass > a}. Unlike \code{sttype}, any number can be given.}
#' }
#' Any other attribute whose name starts with \code{st} is passed through as
#' data and comes back on the node in \code{input$tree}. That includes
#' \code{stclass}, which despite its name is only data -- it is what
#' \code{get_selected(format = "classid")} reports -- and is not applied as a
#' CSS class; use \code{stcssclass} for that.
#' @section Caching:
#' \code{renderTree} can be wrapped in \code{\link[shiny]{bindCache}}, in which
#' case \code{expr} is evaluated only for a cache key that has not been seen
#' before and the cached tree is sent otherwise. It works with
#' \code{\link[shiny]{bindEvent}} as well.
#' @seealso \code{\link{shinyTree}}
#' @export
renderTree <- function(expr, env = parent.frame(), quoted = FALSE){
  func <- shiny::exprToFunction(expr, env, quoted)
  ## Guards against registering the rebind observer more than once for a given
  ## output, since the render function runs again whenever expr invalidates.
  observer_registered <- FALSE
  ## The JSON last sent to the client, kept for the cache and for rebinding.
  last_json <- NULL

  send_tree <- function(shinysession, name, json) {
    last_json <<- json
    updateTree(shinysession, name, json)
    if (!observer_registered && !is.null(shinysession$input)) {
      observer_registered <<- TRUE
      registerRebindObserver(shinysession, name, function() last_json)
    }
  }

  renderFunc <- function(shinysession, name, ...) {
    tree <- func()
    ## expr may return a list or JSON, e.g. from treeToJSON().
    send_tree(shinysession, name, if (is.list(tree)) Rlist2json(tree) else tree)
    NULL
  }

  ## The tree is sent as a message rather than returned as the output value, so
  ## the value bindCache() would store is NULL and replaying it on a cache hit
  ## would leave the tree empty (#109). The hooks store the tree's JSON instead
  ## and send it again on a hit, when the render function itself is skipped.
  ## The write hook runs straight after the render function, so last_json holds
  ## the tree it has just sent.
  cacheWriteHook <- function(value) last_json
  cacheReadHook <- function(json) {
    session <- shiny::getDefaultReactiveDomain()
    send_tree(session, shiny::getCurrentOutputInfo(session)$name, json)
    NULL
  }

  shiny::markRenderFunction(
    shinyTree,
    renderFunc,
    cacheHint = list(label = "renderTree", userExpr = body(func)),
    cacheWriteHook = cacheWriteHook,
    cacheReadHook = cacheReadHook
  )
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
#' @param func function returning the tree contents to resend
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
