#' Render a tree that loads its nodes as they are opened
#'
#' The engine behind \code{renderTree(lazy = TRUE)}. \code{func} evaluates the
#' render expression, which returns the children function -- or a list with
#' \code{children} and, optionally, \code{search}.
#'
#' The children of a node are fetched by the browser with an HTTP request to a
#' URL registered with \code{session$registerDataObj()}, the mechanism Shiny's
#' server-side selectize uses, rather than through the websocket, so a large
#' response does not hold up the session's other messages. Requests are still
#' answered by the one R process that serves every session, so a slow children
#' function delays everyone; see the docs.
#' @noRd
renderTreeLazyFunc <- function(func) {
  observer_registered <- FALSE
  ## The spec built by the last render, so the request handler, which is not a
  ## reactive context, always uses the current children/search functions.
  current <- new.env(parent = emptyenv())
  current$spec <- NULL
  current$generation <- 0L

  renderFunc <- function(shinysession, name, ...) {
    spec <- normaliseLazySpec(func())
    ## The children and search functions are called later, from HTTP requests
    ## outside any reactive context. They are run with the session as the
    ## reactive domain and inside isolate(), so reading an input there -- even
    ## one a factory captured lazily, as in `makeKids(input$folder)` -- works
    ## and returns its current value, rather than raising "Can't access
    ## reactive value outside of reactive consumer". Reads in the render
    ## expression itself are what rebuild the tree.
    current$spec <- spec
    current$session <- shinysession
    current$generation <- current$generation + 1L
    ## Paths the browser has been given and may ask for, so a request cannot
    ## make the children function list a path it was never offered -- e.g.
    ## c("..", "..") in an app that builds file paths from it.
    current$offered <- new.env(parent = emptyenv())
    offerPath(current, character(0))
    markLazyOutput(shinysession, name)
    if (is.null(shinysession$registerDataObj)) {
      stop("renderTree(lazy = TRUE) needs a Shiny session that can serve data ",
           "requests; it cannot be used outside a running app.", call. = FALSE)
    }
    url <- shinysession$registerDataObj(
      paste0("shinyTree-lazy-", name),
      current,
      lazyRequestHandler
    )
    shinysession$sendInputMessage(name, list(
      type = "lazyTree",
      url = url,
      ## Changes whenever the tree is rebuilt, so the client starts afresh and
      ## a stale response from the previous tree is ignored.
      generation = current$generation,
      search = !is.null(spec$search)
    ))
    if (!observer_registered && !is.null(shinysession$input)) {
      observer_registered <<- TRUE
      ## A lazy tree is rebuilt from the URL, so a rebind (#79) resends the
      ## same message; nothing has to be kept on the client.
      registerRebindObserver(shinysession, name, resend = function() {
        shinysession$sendInputMessage(name, list(
          type = "lazyTree", url = url, generation = current$generation,
          search = !is.null(current$spec$search)
        ))
      })
    }
    NULL
  }

  shiny::markRenderFunction(shinyTree, renderFunc, cacheHint = FALSE)
}

#' Check and normalise what a lazy render expression returned
#' @noRd
normaliseLazySpec <- function(x) {
  if (is.function(x)) {
    x <- list(children = x)
  }
  if (!is.list(x) || !is.function(x$children)) {
    stop("With lazy = TRUE, renderTree() must return a function(path) giving a ",
         "node's children, or a list with a `children` function and an ",
         "optional `search` function.", call. = FALSE)
  }
  args <- names(formals(x$children))
  if (!is.primitive(x$children) && !"..." %in% args &&
      (!length(args) || (identical(args, "node")))) {
    stop("The children function must take the node's `path` as its first argument, ",
         "and may also take `node`: function(path) or function(path, node).",
         call. = FALSE)
  }
  if (!is.null(x$search) && !is.function(x$search)) {
    stop("`search` must be a function(text) returning the paths of matching ",
         "nodes.", call. = FALSE)
  }
  list(children = x$children, search = x$search)
}

#' Answer the browser's requests for a lazy tree's children or search results
#'
#' A request carries a JSON body: \code{{"paths": [[...], ...]}} for the
#' children of several nodes at once (massload), or \code{{"search": "text"}}.
#' @noRd
lazyRequestHandler <- function(current, req) {
  respond <- function(status, body) {
    json <- tryCatch(
      as.character(jsonlite::toJSON(body, auto_unbox = TRUE, null = "null")),
      error = function(e) NULL
    )
    if (is.null(json)) {
      status <- 500L
      json <- as.character(jsonlite::toJSON(
        list(error = "The tree's data could not be sent to the browser."), auto_unbox = TRUE))
    }
    shiny::httpResponse(status, "application/json; charset=UTF-8", json,
                        headers = list(`Cache-Control` = "no-store"))
  }
  spec <- current$spec
  if (is.null(spec)) {
    return(respond(503, list(error = "The tree is not ready yet.")))
  }
  body <- tryCatch(readLazyRequest(req), error = function(e) e)
  if (inherits(body, "error")) {
    return(respond(400, list(error = conditionMessage(body))))
  }
  generation <- body$generation
  if (!(is.numeric(generation) && length(generation) == 1 && !is.na(generation) &&
        generation == current$generation)) {
    ## The tree was rebuilt since the browser asked; it will start again.
    return(respond(409, list(error = "stale")))
  }

  if (!is.null(body$search)) {
    result <- runUserCode(current, lazySearch(spec, body$search))
    if (inherits(result, "lazyFailure")) {
      return(respond(500, list(error = cleanUtf8(result$message))))
    }
    for (p in result) offerAncestry(current, p)
    ## I() keeps a one-name path an array rather than a bare string.
    return(respond(200, list(generation = current$generation, matches = lapply(result, I))))
  }

  nodeData <- body$data
  if (is.null(nodeData)) nodeData <- vector("list", length(body$paths))
  ## Each node's answer is serialised once, here, and spliced into the
  ## response. Asking for the same node twice -- the same path and data --
  ## reuses the first answer, so repeating one cheap path cannot make a small
  ## request cost minutes. The answers together are capped in size.
  ## The cap counts every answer in the response, repeats included, so it
  ## bounds what is sent as well as what is computed.
  answered <- list()
  budget <- 20 * 1024^2
  used <- 0
  ## Distinct nodes answered per request; the browser batches what it needs
  ## from one search or restore, which is well under this.
  max_distinct <- 200L
  pieces <- vapply(seq_along(body$paths), function(i) {
    path <- as.character(unlist(body$paths[[i]]))
    ## Keyed on what the children function would be given -- the path and the
    ## server's own data for it -- not on the data the browser sent, which
    ## would let junk data defeat the reuse.
    used_data <- if (isOffered(current, path)) offeredData(current, path, nodeData[[i]]) else list()
    key <- paste(pathKey(path), dataKey(used_data))
    piece <- answered[[key]]
    if (is.null(piece)) {
      piece <- if (used > budget || length(answered) >= max_distinct) tooMuch(path)
               else lazyAnswer(current, spec, path, nodeData[[i]])
      answered[[key]] <<- piece
    }
    used <<- used + nchar(piece, type = "bytes")
    if (used > budget) tooMuch(path) else piece
  }, character(1))
  json <- paste0('{"generation":', current$generation, ',"nodes":[', paste(pieces, collapse = ","), "]}")
  shiny::httpResponse(200L, "application/json; charset=UTF-8", enc2utf8(json),
                      headers = list(`Cache-Control` = "no-store"))
}

#' One node's answer, as JSON: its children, or an error
#' @noRd
lazyAnswer <- function(current, spec, path, wanted) {
  if (!isOffered(current, path)) {
    return(nodeJson(path, error = "This node is not in the tree."))
  }
  ## The node's st* data as the server sent it, not as the browser says it
  ## is: a request cannot make the children function see an stid it never
  ## gave out.
  kids <- runUserCode(current, lazyChildren(spec, path, offeredData(current, path, wanted)))
  if (inherits(kids, "lazyFailure")) {
    ## req(FALSE) means "not yet": the node stays closed and can be opened
    ## again, with no error, as Shiny cancels an output silently.
    if (isTRUE(kids$silent)) return(nodeJson(path, error = "", silent = TRUE))
    return(nodeJson(path, error = kids$message))
  }
  ## Serialised on its own, so one bad node fails only its own branch, with
  ## numbers at full precision so an stid like 1.00001 is not sent as 1.
  kidsJson <- tryCatch(
    as.character(jsonlite::toJSON(cleanUtf8(kids), auto_unbox = TRUE, null = "null", digits = NA)),
    error = function(e) NULL)
  if (is.null(kidsJson)) {
    return(nodeJson(path, error = "A child could not be sent to the browser; check its attributes."))
  }
  ## The parent's children are what was just sent, so forget what an earlier
  ## fetch of the same parent offered for them; otherwise data that changes on
  ## every fetch (a timestamp, say) would pile up for as long as the tree is
  ## shown.
  ## Each path is cleared once, before any child is offered, so same-named
  ## siblings -- which share a path -- are all kept.
  for (p in unique(lapply(kids, function(kid) as.character(kid$data$stpath)))) clearOffered(current, p)
  for (kid in kids) offerNested(current, path, kid)
  paste0('{"path":', as.character(jsonlite::toJSON(I(path))), ',"children":', kidsJson, "}")
}

tooMuch <- function(path) {
  nodeJson(path, error = "Too much was asked for at once; open fewer nodes together.")
}

nodeJson <- function(path, error, silent = FALSE) {
  body <- list(path = I(path), error = cleanUtf8(error))
  if (silent) body$silent <- TRUE
  as.character(jsonlite::toJSON(body, auto_unbox = TRUE))
}

#' Make every string in x valid UTF-8, showing bad bytes as "?"
#'
#' A file name in another encoding, say, would otherwise make the response
#' invalid JSON -- in a name, an attribute or an error message alike.
#' @noRd
cleanUtf8 <- function(x) {
  if (is.character(x)) {
    bad <- !is.na(x) & !validUTF8(x)
    if (any(bad)) x[bad] <- iconv(x[bad], "UTF-8", "UTF-8", sub = "?")
    return(x)
  }
  if (is.list(x)) {
    at <- attributes(x)
    x <- lapply(x, cleanUtf8)
    attributes(x) <- at
    if (!is.null(names(x))) names(x) <- cleanUtf8(names(x))
  }
  x
}

#' Run the user's children or search code for a request
#'
#' With the session as the reactive domain and inside isolate(), so the code
#' can read inputs. Any condition that ends it -- an error, or a bare
#' condition signalled with stop() -- becomes a failure for that request, and
#' its message is only passed to the browser when Shiny would show it: not
#' with options(shiny.sanitize.errors = TRUE), unless it is a safeError().
#' @noRd
runUserCode <- function(current, expr) {
  session <- current$session
  run <- function() {
    if (is.null(session)) shiny::isolate(expr)
    else shiny::withReactiveDomain(session, shiny::isolate(expr))
  }
  ## Errors end the code and fail this request. So does stop() given a bare
  ## condition, which is not an error: nothing handles it, and R then aborts
  ## through the "abort" restart, which is caught here. Every other signal --
  ## dplyr's informational ones, rlang::signal(), a user's own -- is left
  ## alone, as it is everywhere else in R. Warnings and messages are printed
  ## and the code carries on. An interrupt (Ctrl-C) is not caught, so a slow
  ## children function can still be stopped from the console.
  last <- NULL
  withRestarts(
    tryCatch(
      withCallingHandlers(
        run(),
        warning = function(w) {
          ## With options(warn = 2) a warning is an error, and R raises it as
          ## one once the warning's handlers have run -- so it is left alone.
          if (getOption("warn", 0) >= 2) return()
          cat("Warning in renderTree(lazy = TRUE): ", conditionMessage(w), "\n",
              sep = "", file = stderr())
          invokeRestart("muffleWarning")
        },
        message = function(m) {
          cat(conditionMessage(m), file = stderr())
          invokeRestart("muffleMessage")
        },
        condition = function(c) {
          ## Remembered only when stop() raised it, since that is the one case
          ## in which the abort that follows is its doing. A signal raised any
          ## other way -- rlang::signal(), signalCondition() -- is over once its
          ## handlers return, and must not be blamed for an abort later on.
          if (!inherits(c, c("warning", "message", "error", "interrupt"))) {
            callers <- vapply(sys.calls(), function(cl) paste(deparse(cl[[1]]), collapse = ""), "")
            last <<- if (any(callers %in% c("stop", "base::stop"))) c else NULL
          }
        }
      ),
      error = function(cond) lazyFailure(cond)
    ),
    abort = function() lazyFailure(if (is.null(last)) simpleError("The code was aborted.") else last)
  )
}

## stop() given a bare condition prints "Error: <message>" itself before it
## aborts; lazyFailure() then reports it again. Nothing in R lets that first
## line be suppressed without also hiding real errors elsewhere, so it is
## left, and lazyFailure() names the condition's class to tell them apart.

lazyFailure <- function(cond) {
  msg <- conditionMessage(cond)
  ## req() and validate() are Shiny's way of saying "not yet": silent, as in a
  ## render function, so nothing is printed for them.
  if (!inherits(cond, "shiny.silent.error")) {
    ## Also report it in the R console, where the app's author will look; the
    ## browser only gets it on input$<id>_lazy_error and in its console.
    cat("Error in renderTree(lazy = TRUE): ",
        if (length(msg) && nzchar(msg)) msg else class(cond)[1], "\n",
        sep = "", file = stderr())
  }
  structure(list(message = userErrorMessage(cond),
                 silent = inherits(cond, "shiny.silent.error") && !nzchar(msg)),
            class = "lazyFailure")
}

userErrorMessage <- function(cond) {
  msg <- conditionMessage(cond)
  ## As in Shiny: validate()/need() messages and safeError()s are shown even
  ## when errors are sanitised; req() has no message at all.
  if (isTRUE(getOption("shiny.sanitize.errors")) &&
      !inherits(cond, c("shiny.custom.error", "shiny.silent.error"))) {
    return("An error has occurred. Check your logs or contact the app author for clarification.")
  }
  if (!length(msg) || !nzchar(msg)) "An error has occurred." else msg
}

#' Paths the browser has been given
#'
#' Kept per render in an environment, keyed by the path as a JSON array, so
#' no two paths share a key -- c("a", "b") and "a\u0001b" do not, nor do the
#' root and a path of one empty name.
#' @noRd
pathKey <- function(path) as.character(jsonlite::toJSON(as.character(path)))
## Data is compared as JSON, the form it travels in: a number the server sent
## as a double comes back from the browser as an integer.
dataKey <- function(data) as.character(jsonlite::toJSON(data, auto_unbox = TRUE, null = "null", digits = NA))
offerPath <- function(current, path, data = list()) {
  key <- pathKey(path)
  ## Same-named siblings share a path; keep each one's data.
  prev <- get0(key, envir = current$offered, inherits = FALSE)
  entry <- if (is.list(prev)) prev else list()
  if (length(data)) {
    dk <- tryCatch(dataKey(data), error = function(e) NULL)
    if (!is.null(dk) && !dk %in% names(entry)) {
      entry[[dk]] <- data
      ## One entry per same-named sibling is all that is needed. Data that
      ## changes on every fetch (a timestamp, say) would otherwise grow this
      ## for as long as the tree is shown; keep the most recent ones.
      if (length(entry) > 50L) entry <- entry[seq.int(length(entry) - 49L, length(entry))]
    }
  }
  assign(key, entry, envir = current$offered)
}
clearOffered <- function(current, path) {
  key <- pathKey(path)
  if (exists(key, envir = current$offered, inherits = FALSE)) assign(key, list(), envir = current$offered)
}
offeredData <- function(current, path, wanted = NULL) {
  entry <- get0(pathKey(path), envir = current$offered, inherits = FALSE)
  if (!length(entry)) return(list())
  if (length(entry) > 1 && !is.null(wanted)) {
    ## Several same-named siblings: the one whose data the browser named, if
    ## it is one the server gave out; never data it did not.
    wk <- tryCatch(dataKey(wanted), error = function(e) NULL)
    if (!is.null(wk) && wk %in% names(entry)) return(entry[[wk]])
  }
  entry[[1]]
}
isOffered <- function(current, path) exists(pathKey(path), envir = current$offered, inherits = FALSE)
#' Only branches are offered -- nodes sent with children to fetch, or with
#' children inline. A leaf has no children to ask for, so a request naming one
#' (e.g. a symbolic link the app shows as a leaf, which an app building file
#' paths would follow) is refused like any other unknown path.
#' @noRd
offerNested <- function(current, parent, kid) {
  path <- as.character(kid$data$stpath)
  data <- kid$data[grepl("^st", names(kid$data)) & names(kid$data) != "stpath"]
  if (isTRUE(kid$children)) {
    offerPath(current, path, data)
  } else if (is.list(kid$children)) {
    offerPath(current, path, data)
    for (k in kid$children) offerNested(current, path, k)
  }
}
#' The branches leading to a search match -- its ancestors, not the match
#' itself, which is offered only when the browser is sent it as a branch.
#' @noRd
offerAncestry <- function(current, path) {
  path <- as.character(unlist(path))
  for (i in seq_len(length(path) - 1L)) offerPath(current, path[seq_len(i)])
}

#' Parse a lazy tree request body
#' @noRd
readLazyRequest <- function(req) {
  if (!identical(req$REQUEST_METHOD, "POST")) {
    stop("Expected a POST request.")
  }
  raw <- req$rook.input$read()
  if (!length(raw)) {
    stop("Empty request.")
  }
  body <- jsonlite::fromJSON(rawToChar(raw), simplifyVector = FALSE)
  if (!is.list(body) || (is.null(body$paths) && is.null(body$search))) {
    stop("The request must give `paths` or `search`.")
  }
  if (!is.null(body$search) && !(is.character(body$search) && length(body$search) == 1)) {
    stop("`search` must be a single string.")
  }
  if (!is.null(body$paths)) {
    if (!is.list(body$paths) || !is.null(names(body$paths)) || length(body$paths) > 1000) {
      stop("`paths` must be an array of at most 1000 paths.")
    }
    ok <- vapply(body$paths, function(p) {
      is.list(p) && is.null(names(p)) &&
        all(vapply(p, function(x) is.character(x) && length(x) == 1, logical(1)))
    }, logical(1))
    if (!all(ok)) {
      stop("Each path must be a list of node names.")
    }
    if (!is.null(body$data)) {
      if (!is.list(body$data) || !is.null(names(body$data)) ||
          length(body$data) != length(body$paths) ||
          !all(vapply(body$data, function(d) is.list(d) && (!length(d) || !is.null(names(d))), logical(1)))) {
        stop("`data` must be an array with one object per path.")
      }
      ## Only the node's own st* data is passed on.
      body$data <- lapply(body$data, function(d) d[grepl("^st", names(d))])
    }
  }
  body
}

#' The children of one node, as jsTree JSON nodes
#'
#' Calls the user's children function and converts its named list the same way
#' renderTree() converts a whole tree. A child that has children of its own is
#' marked stchildren = TRUE, or is a non-empty list; the first is fetched when
#' opened, the second is sent with its children now.
#'
#' Each node records the path it was fetched under in its data, as stpath, so
#' the browser asks for the same path if the node is later renamed or moved.
#' @noRd
lazyChildren <- function(spec, path, data = list()) {
  kids <- callChildren(spec$children, path, data)
  if (is.null(kids) || !length(kids)) {
    return(list())
  }
  if (is.data.frame(kids)) {
    stop("The children function returned a data frame. Return a named list of ",
         "children, as renderTree() takes; dfToTree() converts a data frame to one.")
  }
  if (!is.list(kids) || is.object(kids) || is.null(names(kids)) ||
      any(is.na(names(kids))) || any(!nzchar(names(kids)))) {
    stop("The children function must return a named list, as renderTree() takes, ",
         "with a non-empty name for every child.")
  }
  lazyNodes(kids, path)
}

#' Call the children function, giving it the node's data if it asks for it
#'
#' The data is passed by name, and only to an argument called `node`:
#' function(path) and function(path, node) both work, and a function with
#' some other second argument, a default for instance, is left alone.
#' @noRd
callChildren <- function(f, path, data) {
  path <- decodePathName(path)
  if ("node" %in% names(formals(f))) f(path, node = data) else f(path)
}

lazyNodes <- function(tree, parent) {
  nm <- names(tree)
  ## A name that is not valid UTF-8 (e.g. a file name in another encoding)
  ## would make the response invalid JSON, so it is shown with its bad bytes
  ## as "?". Its path keeps the name byte for byte, written as \x escapes, so
  ## two names differing only in those bytes stay apart and the children
  ## function is given the original back (see decodePathName()).
  bad <- !is.na(nm) & !validUTF8(nm)
  pathNames <- nm
  if (any(bad)) {
    pathNames[bad] <- vapply(nm[bad], encodePathName, "")
    nm[bad] <- iconv(nm[bad], "UTF-8", "UTF-8", sub = "?")
  }
  lapply(seq_along(tree), function(i) {
    node <- tree[[i]]
    path <- c(parent, pathNames[i])
    json <- flatNode(node, nm[i], id = NULL, parent = NULL)
    json$id <- NULL
    json$parent <- NULL
    hasKids <- attributes(node)$stchildren
    json$data$stchildren <- NULL
    if (!is.null(hasKids) && !(is.logical(hasKids) && length(hasKids) == 1 && !is.na(hasKids))) {
      warning(sprintf(paste0("stchildren on \"%s\" should be TRUE or FALSE, not %s; ",
                             "treating the node as a leaf."),
                      paste(path, collapse = " / "), deparse(hasKids)[1]), call. = FALSE)
      hasKids <- FALSE
    }
    if (!is.null(json$data$stpath)) {
      warning(sprintf(paste0("The stpath attribute on \"%s\" is replaced: renderTree(lazy = TRUE) ",
                             "uses it for the path the node was fetched under."),
                      paste(path, collapse = " / ")), call. = FALSE)
    }
    json$data$stpath <- I(path)
    if (is.list(node) && length(node)) {
      json$children <- lazyNodes(node, path)
    } else if (isTRUE(hasKids)) {
      json$children <- TRUE
    }
    json
  })
}

#' Search a lazy tree
#'
#' Returns the paths of matching nodes, as a list of character vectors. With a
#' search function the user's code finds them; otherwise every branch is
#' walked with the children function and node names are matched
#' case-insensitively, as jsTree's own search does.
#'
#' The walk is breadth-first over a queue kept as a preallocated list with a
#' read index, so it is linear in the nodes visited. \code{max_calls} bounds
#' the calls to the children function and \code{max_nodes} the nodes looked
#' at, so a huge or endless tree stops with a clear error instead of tying up
#' the R process, which serves every session.
#' @noRd
lazySearch <- function(spec, text, current = NULL, limit = 1000L,
                       max_nodes = 100000L, max_calls = 10000L, max_seconds = 5) {
  if (!nzchar(text)) {
    return(list())
  }
  if (!is.null(spec$search)) {
    return(normaliseSearchResult(spec$search(text), limit))
  }
  tooBig <- function() {
    stop("The tree is too large or slow to search by walking it. Give renderTree(lazy = TRUE) ",
         "a `search` function, e.g. list(children = <function>, search = <function>).",
         call. = FALSE)
  }
  needle <- tolower(text)
  ## Calls are counted, but a slow children function could still keep R busy
  ## for a long time within that count, so the walk also has a time budget.
  deadline <- Sys.time() + max_seconds
  found <- vector("list", 64L)
  nFound <- 0L
  visited <- 0L
  calls <- 0L
  ## Each queue entry is a branch: its path, and its children if they were
  ## given inline (NULL means ask the children function).
  queue <- vector("list", 1024L)
  queue[[1]] <- list(path = character(0), kids = NULL, data = list())
  failed <- 0L
  head <- 1L
  tail <- 1L
  while (head <= tail && nFound < limit) {
    item <- queue[[head]]
    queue[head] <- list(NULL)
    head <- head + 1L
    kids <- item$kids
    if (is.null(kids)) {
      calls <- calls + 1L
      if (calls > max_calls || Sys.time() > deadline) tooBig()
      ## A branch whose children cannot be listed is skipped, so one failing
      ## branch does not stop the rest of the tree being searched.
      kids <- tryCatch(callChildren(spec$children, item$path, item$data),
                       error = function(e) {
                         failed <<- failed + 1L
                         NULL
                       })
    }
    if (!length(kids)) next
    if (!is.list(kids) || is.null(names(kids))) {
      stop("The children function must return a named list, as renderTree() takes.",
           call. = FALSE)
    }
    raw <- names(kids)
    nm <- cleanUtf8(raw)
    hit <- grepl(needle, tolower(nm), fixed = TRUE)
    badName <- !is.na(raw) & !validUTF8(raw)
    pathNm <- raw
    if (any(badName)) pathNm[badName] <- vapply(raw[badName], encodePathName, "")
    for (i in seq_along(kids)) {
      visited <- visited + 1L
      if (visited > max_nodes) tooBig()
      child <- c(item$path, pathNm[i])
      if (isTRUE(hit[i])) {
        nFound <- nFound + 1L
        if (nFound > length(found)) length(found) <- 2L * length(found)
        found[[nFound]] <- child
        if (nFound >= limit) break
      }
      node <- kids[[i]]
      data <- stAttributes(node)
      branch <- if (is.list(node) && length(node)) list(path = child, kids = node, data = data)
                else if (isTRUE(attributes(node)$stchildren)) list(path = child, kids = NULL, data = data)
      if (!is.null(branch)) {
        tail <- tail + 1L
        if (tail > length(queue)) length(queue) <- 2L * length(queue)
        queue[[tail]] <- branch
      }
    }
  }
  if (failed > 0L && nFound == 0L) {
    stop(sprintf("Nothing matched, and %d branch%s could not be searched because the ",
                 failed, if (failed == 1L) "" else "es"),
         "children function failed for it.", call. = FALSE)
  }
  found[seq_len(nFound)]
}

#' The st* attributes of a node, as the children function receives them
#' @noRd
stAttributes <- function(node) {
  a <- attributes(node)
  a <- a[grepl("^st", names(a)) & !names(a) %in% c("stchildren", "stpath")]
  if (is.null(a)) list() else a
}

#' Check what a user's search function returned
#'
#' Accepts a list of paths (character vectors) or a character vector of
#' single-name paths, and returns an unnamed list of character vectors, so it
#' always serialises as an array of arrays.
#' @noRd
normaliseSearchResult <- function(paths, limit) {
  bad <- function() {
    stop("The search function must return a list of paths, each a character vector ",
         "of node names from the root, e.g. list(c(\"Europe\", \"France\")).", call. = FALSE)
  }
  if (is.null(paths) || !length(paths)) {
    return(list())
  }
  if (is.character(paths) && is.null(dim(paths))) {
    paths <- as.list(paths)
  }
  if (!is.list(paths) || is.data.frame(paths)) bad()
  paths <- unname(utils::head(paths, limit))
  lapply(paths, function(p) {
    if (!is.character(p) || !length(p) || anyNA(p) || any(!nzchar(p))) bad()
    unname(p)
  })
}

#' Remember which outputs of a session are lazy trees
#'
#' So updateTree() and expandTree(), which cannot work on one, can say so
#' rather than replacing it or fetching it whole.
#' @noRd
#' The render function is given the full, namespaced output name and the root
#' session; updateTree() is given the module-local id and the module's
#' session, so it namespaces the id through that session before looking it up.
#' @noRd
markLazyOutput <- function(session, name) {
  ud <- session$userData
  if (is.null(ud)) return(invisible())
  if (is.null(ud$shinyTreeLazy)) ud$shinyTreeLazy <- character()
  ud$shinyTreeLazy <- union(ud$shinyTreeLazy, name)
  invisible()
}
isLazyOutput <- function(session, treeId) {
  ud <- session$userData
  if (is.null(ud) || is.null(ud$shinyTreeLazy)) return(FALSE)
  full <- if (is.function(session$ns)) session$ns(treeId) else treeId
  ## A mock or root session's ns() prefixes its own token; match either form.
  any(c(full, treeId) %in% ud$shinyTreeLazy)
}
stopIfLazy <- function(session, treeId, what, instead) {
  if (isLazyOutput(session, treeId)) {
    stop(sprintf("%s() cannot be used on \"%s\", which is rendered with renderTree(lazy = TRUE). %s",
                 what, treeId, instead), call. = FALSE)
  }
}

#' Names that are not valid UTF-8, carried in a path
#'
#' Written as "\\x" followed by the name's bytes in hex, prefixed so the form
#' cannot be mistaken for an ordinary name; decodePathName() turns it back
#' into the original bytes before the children function sees it.
#' @noRd
encodePathName <- function(x) {
  paste0("\u0002x", paste(sprintf("%02x", as.integer(charToRaw(x))), collapse = ""))
}
decodePathName <- function(x) {
  enc <- startsWith(x, "\u0002x")
  if (!any(enc)) return(x)
  x[enc] <- vapply(x[enc], function(e) {
    hex <- substring(e, 3)
    if (!nzchar(hex) || nchar(hex) %% 2 != 0 || grepl("[^0-9a-f]", hex)) return(e)
    rawToChar(as.raw(strtoi(substring(hex, seq(1, nchar(hex), 2), seq(2, nchar(hex), 2)), 16L)))
  }, "")
  x
}
