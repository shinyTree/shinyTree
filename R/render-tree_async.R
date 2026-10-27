#' Render a ShinyTree asynchronously
#'
#' Like \code{\link{renderTree}}, but \code{expr} may also return a promise
#' (see the \pkg{promises} package) of the tree, for a tree that is slow to
#' build: with \pkg{future}, it is built in another process and the app stays
#' responsive in the meantime. The tree is sent once the promise resolves; if
#' it is rejected, the error is shown in place of the tree. An expression that
#' returns a tree straight away works as it does in \code{renderTree}.
#'
#' Lazy loading (\code{renderTree(lazy = TRUE)}) is not available here.
#'
#' @param expr An expression that returns a nested list or JSON, as for
#'   \code{\link{renderTree}}, or a promise of one.
#' @param env The environment in which \code{expr} should be evaluated.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#' is useful if you want to save an expression in a variable.
#' @return A render function, to assign to \code{output$<id>} for a
#'   \code{\link{shinyTree}} with that id.
#' @examples
#' if (interactive() && requireNamespace("future", quietly = TRUE)) {
#'   library(shiny)
#'   library(promises)
#'   library(future)
#'   plan(multisession)
#'
#'   ui <- fluidPage(shinyTree("tree"))
#'   server <- function(input, output, session) {
#'     output$tree <- renderTreeAsync({
#'       future({
#'         Sys.sleep(2)  # a tree that is slow to build
#'         list(root = list(leaf = ""))
#'       })
#'     })
#'   }
#'   shinyApp(ui, server)
#' }
#'
#' # See also the 16-async example:
#' if (interactive()) {
#'   shiny::runApp(system.file("examples/16-async", package = "shinyTree"))
#' }
#' @seealso \code{\link{renderTree}}, \code{\link{shinyTree}}
#' @export
renderTreeAsync <- function(expr, env = parent.frame(), quoted = FALSE){
  func <- shiny::exprToFunction(expr, env, quoted)
  treeRenderFunction(func, "renderTreeAsync", async = TRUE)
}
