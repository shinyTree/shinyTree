#' Render a ShinyTree
#'
#' Should return a list from the given expression which will be converted into a
#' \code{\link{shinyTree}}.
#'
#' @param expr The expression to be evaluated which should produce a list.
#' @param env The environment in which \code{expr} should be evaluated.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#' is useful if you want to save an expression in a variable.
#' @param lazy If \code{TRUE}, the tree's nodes are fetched as they are opened
#' rather than all at once, for trees too large to send whole. \code{expr}
#' then returns a function giving a node's children; see \emph{Lazy loading}.
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
#' @section Lazy loading:
#' With \code{lazy = TRUE}, \code{expr} returns a function that is given a
#' node's \code{path} -- the names from the root down to it,
#' \code{character(0)} for the top level -- and returns that node's children as
#' a named list, in the same form \code{renderTree()} takes. Only the top level
#' is sent when the tree is shown; each node's children are fetched when the
#' user opens it.
#' \preformatted{
#' output$tree <- renderTree(lazy = TRUE, {
#'   root <- input$folder          # reactive: a change reloads the whole tree
#'   function(path) {
#'     dir <- do.call(file.path, as.list(c(root, path)))
#'     subdirs <- list.dirs(dir, full.names = FALSE, recursive = FALSE)
#'     # A symbolic link can point back up the tree; show it as a leaf.
#'     links <- nzchar(Sys.readlink(file.path(dir, subdirs)))
#'     files <- setdiff(list.files(dir), subdirs[!links])
#'     c(setNames(lapply(subdirs[!links], function(d) structure("", stchildren = TRUE)),
#'                subdirs[!links]),
#'       setNames(lapply(files, function(f) ""), files))
#'   }
#' })
#' }
#' \describe{
#'   \item{\code{stchildren = TRUE}}{marks a child that has children of its own,
#'     so it shows as closed and is fetched when opened. A child given as a
#'     non-empty list is sent with its children straight away instead. Any value
#'     other than \code{TRUE} or \code{FALSE} is treated as \code{FALSE}, with a
#'     warning.}
#'   \item{\code{function(path, node)}}{The function may take an argument
#'     named \code{node}: the \code{st*} attributes of the node being opened,
#'     as a list. Siblings with the same name have the same path, so give each
#'     an id, \code{structure("", stchildren = TRUE, stid = 42)}, and use
#'     \code{node$stid} to tell them apart. Only an argument called
#'     \code{node} receives it. \code{stpath} is used by shinyTree for the path
#'     a node was fetched under, so do not set it yourself.}
#'   \item{Node attributes}{All the attributes below work on the children,
#'     including \code{stselected} and \code{stopened}; an opened child's
#'     children are fetched at once.}
#'   \item{Reactivity}{\code{expr} is evaluated as usual, so anything reactive
#'     it reads rebuilds the tree when it changes, which starts it again from
#'     the top level. The children function can read reactive values too, and
#'     gets their current value, but reading one there does not rebuild the
#'     tree.}
#'   \item{Errors}{An error in the children function is shown in the R console,
#'     the node it was opening stays closed so opening it again retries, and
#'     \code{input$<outputId>_lazy_error} is set to a list with the node's
#'     \code{path} and the \code{message}. With
#'     \code{options(shiny.sanitize.errors = TRUE)} the message is replaced by
#'     a generic one, as for other outputs, unless it is a
#'     \code{\link[shiny]{safeError}}.}
#'   \item{Search}{With \code{shinyTree(search = TRUE)} the whole tree is
#'     searched, not just the nodes fetched so far: the server finds the
#'     matches, and the branches leading to them are fetched and opened. By
#'     default this walks the whole tree with the children function, matching
#'     names case-insensitively, and gives up with an error after 10,000 calls
#'     or 100,000 nodes or 5 seconds. A branch whose children function fails
#'     is skipped. For anything larger, or slow, return
#'     \code{list(children = <function>, search = <function>)} instead, where
#'     \code{search} is given the search text and returns the paths of the
#'     matching nodes as a list of character vectors, e.g. from a database. At
#'     most 1,000 matches are shown.}
#'   \item{\code{input$tree}}{holds the nodes fetched so far. Nodes can only be
#'     selected or ticked once they are fetched, so \code{get_selected()} and
#'     \code{get_checked()} are complete -- except that ticking a branch that has
#'     never been opened reports the branch, not the children it has not
#'     fetched, so \code{leaves_only = TRUE} cannot list them.}
#'   \item{State}{With \code{shinyTree(state = TRUE)}, the branches that were
#'     open, and the parents of the nodes that were selected or ticked, are
#'     fetched and reopened after a reload. Of several siblings with the same
#'     name, the same one is reopened, by its position among them.}
#'   \item{Renaming and moving}{A node keeps the path it was fetched under, so
#'     after the user renames or moves it with the context menu or drag and
#'     drop, opening it still asks for its children by its original path.}
#'   \item{Speed}{One R process answers every user of the app, so a children
#'     function that takes a second makes everyone wait a second. Keep it fast:
#'     query only the one level asked for, and cache expensive results.}
#'   \item{Trust}{The server only asks the children function for branches it
#'     has sent to that browser -- never for a leaf, and never for a path it
#'     made up, such as \code{c("..", "..")} -- and \code{node} is always the
#'     data the server sent with that branch, never what the browser says it
#'     is. It cannot know what a name means to the app, though: if the app
#'     turns names into file paths or database keys, it is still up to the app
#'     not to offer a branch, such as a symbolic link, that leads somewhere it
#'     should not.}
#'   \item{Not available}{\code{bindCache()}; \code{updateTree()}, which stops
#'     with an error -- change what \code{expr} returns to rebuild the tree; and
#'     \code{expandTree()}, which stops with an error since it would have to
#'     fetch the whole tree.}
#' }
#' Several branches that need fetching at once -- on a search or a state
#' restore -- are fetched in a single request. The \code{27-lazy} example
#' shows a million-node tree with a \code{search} function, and a folder
#' browser.
#' @section Caching:
#' \code{renderTree} can be wrapped in \code{\link[shiny]{bindCache}}, in which
#' case \code{expr} is evaluated only for a cache key that has not been seen
#' before and the cached tree is sent otherwise. It works with
#' \code{\link[shiny]{bindEvent}} as well.
#' @seealso \code{\link{shinyTree}}
#' @export
renderTree <- function(expr, env = parent.frame(), quoted = FALSE, lazy = FALSE){
  func <- shiny::exprToFunction(expr, env, quoted)
  if (isTRUE(lazy)) {
    return(renderTreeLazyFunc(func))
  }
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
    if (is.function(tree)) {
      stop("renderTree()'s expression returned a function. To load the tree's ",
           "nodes as they are opened, use renderTree(lazy = TRUE, ...).", call. = FALSE)
    }
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
#' @param resend function that resends the tree itself, used instead of
#'   \code{func} by lazy trees
#' @noRd
registerRebindObserver <- function(session, treeId, func = NULL, resend = NULL) {
  rebindId <- paste0(treeId, "_rebind")
  shiny::observeEvent(session$input[[rebindId]],
                      {
                        if (is.null(resend)) updateTree(session, treeId, func()) else resend()
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
