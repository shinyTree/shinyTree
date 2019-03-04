#' Render an asynchronous ShinyTree 
#' 
#' Should return a list from the given expression which will be converted into a
#' \code{\link{shinyTree}}.
#' 
#' @importFrom promises then is.promising with_promise_domain new_promise_domain
#' @importFrom shiny exprToFunction getDefaultReactiveDomain markRenderFunction
#' @importFrom htmlwidgets shinyRenderWidget
#' 
#' @param expr The expression to be evaluated which should produce a list.
#' @param env The environment in which \code{expr} should be evaluated.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#' is useful if you want to save an expression in a variable.
#' @seealso \code{\link{shinyTree}}
#' @export
renderTreeAsync <- function(expr, env = parent.frame(), quoted = FALSE){
  if (!quoted) expr = substitute(expr)
  
  outputInfoEnv = new.env(parent = emptyenv())
  outputInfoEnv[["outputName"]] = NULL
  outputInfoEnv[["session"]] = NULL

  exprFunc = shiny::exprToFunction(expr, env, quoted = TRUE)

  processWidget = function(instance, shinysession, name, ...) {
    updateTree(session = shiny::getDefaultReactiveDomain(), treeId = outputInfoEnv[["outputName"]], data = instance)
  }
  
  widgetFunc = function() {
    instance = exprFunc()
    if (promises::is.promising(instance)) {
      promises::then(instance, processWidget)
    } else {
      processWidget(instance)
    }
  }

  renderFunc = htmlwidgets::shinyRenderWidget(
    widgetFunc(), shinyTree, environment(), FALSE
  )

  func = shiny::markRenderFunction(shinyTree, function(shinysession, name, ...) {
    domain = tempVarsPromiseDomain(outputInfoEnv, outputName = name, session = shinysession)
    promises::with_promise_domain(domain, renderFunc())
  })

  func
}


getAll = function(x, env) {
  as.list(mget(x, env, ifnotfound = rep(list(NULL), times = length(x))))
}

setAll = function(lst, env) {
  mapply(names(lst), lst, FUN = function(name, val) {
    assign(name, val, env)
  })
  invisible()
}

tempVarsPromiseDomain = function(env, ...) {
  force(env)
  vars = list(...)

  promises::new_promise_domain(
    wrapOnFulfilled = function(onFulfilled) {
      # force(onFulfilled)
      function(...) {
        old = getAll(names(vars), env)
        setAll(vars, env)
        on.exit({
          setAll(old, env)
        }, add = TRUE)

        onFulfilled(...)
      }
    },
    wrapOnRejected = function(onRejected) {
      # force(onRejected)
      function(...) {
        old = getAll(names(vars), env)
        setAll(vars, env)
        on.exit({
          setAll(old, env)
        }, add = TRUE)

        onRejected(...)
      }
    },
    wrapSync = function(expr) {
      old = getAll(names(vars), env)
      setAll(vars, env)
      on.exit({
        setAll(old, env)
      }, add = TRUE)

      force(expr)
    }
  )
}
