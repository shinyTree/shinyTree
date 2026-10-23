context("test_lazy")

## The server side of renderTree(lazy = TRUE) (#11): the request handler that
## answers the browser, called directly with fake requests.

fake_request <- function(body, method = "POST") {
  raw <- if (is.character(body)) charToRaw(body)
         else charToRaw(as.character(jsonlite::toJSON(body, auto_unbox = TRUE, digits = NA)))
  list(REQUEST_METHOD = method,
       rook.input = list(read = function(...) raw))
}
## The handler only answers for paths it has sent to the browser; tests that
## ask for deeper paths directly list them in `offer`.
lazy_state <- function(children, search = NULL, generation = 1L,
                       offer = list(character(0), "a", c("a", "a2"), c("c", "c2"), "bad")) {
  st <- new.env(parent = emptyenv())
  st$spec <- normaliseLazySpec(list(children = children, search = search))
  st$generation <- generation
  st$session <- NULL
  st$offered <- new.env(parent = emptyenv())
  for (p in offer) offerPath(st, p)
  st
}
call_handler <- function(state, body, method = "POST") {
  ## Errors from the user's code are also printed to the R console.
  res <- suppressMessages(utils::capture.output(
    out <- lazyRequestHandler(state, fake_request(body, method)), type = "message"))
  list(status = out$status, body = jsonlite::fromJSON(out$content, simplifyVector = FALSE))
}

tree_kids <- function(path) {
  key <- paste(path, collapse = "/")
  if (!nzchar(key)) {
    return(list(a = structure("", stchildren = TRUE), b = "",
                c = list(c1 = "", c2 = structure("", stchildren = TRUE))))
  }
  switch(key,
    "a" = list(a1 = structure("", sticon = "fa fa-file", stinfo = "x"),
               a2 = structure("", stchildren = TRUE, stselected = TRUE)),
    "a/a2" = list(deep = ""),
    "c/c2" = list(c2x = ""),
    list())
}

test_that("children come back as jsTree nodes, with stchildren marking branches", {
  r <- call_handler(lazy_state(tree_kids), list(paths = list(list()), generation = 1))
  expect_equal(r$status, 200L)
  top <- r$body$nodes[[1]]$children
  expect_equal(vapply(top, `[[`, "", "text"), c("a", "b", "c"))
  expect_true(top[[1]]$children)              # load on open
  expect_null(top[[2]]$children)              # leaf
  ## Children given inline are sent nested, straight away.
  expect_equal(vapply(top[[3]]$children, `[[`, "", "text"), c("c1", "c2"))
  expect_true(top[[3]]$children[[2]]$children)
  ## stchildren is not passed through as data.
  expect_null(top[[1]]$data$stchildren)
})

test_that("node attributes are honoured on fetched children", {
  r <- call_handler(lazy_state(tree_kids), list(paths = list(list("a")), generation = 1))
  kids <- r$body$nodes[[1]]$children
  expect_equal(kids[[1]]$icon, "fa fa-file")
  expect_equal(kids[[1]]$data$stinfo, "x")
  expect_true(kids[[2]]$state$selected)
})

test_that("several paths are answered in one request, in order (massload)", {
  r <- call_handler(lazy_state(tree_kids),
                    list(paths = list(list("a"), list("a", "a2"), list("c", "c2")), generation = 1))
  expect_equal(r$status, 200L)
  expect_equal(lapply(r$body$nodes, function(n) unlist(n$path)),
               list("a", c("a", "a2"), c("c", "c2")))
  expect_equal(r$body$nodes[[2]]$children[[1]]$text, "deep")
  expect_equal(r$body$nodes[[3]]$children[[1]]$text, "c2x")
})

test_that("an error in the children function is reported for that node only", {
  kids <- function(path) if (identical(path, "bad")) stop("database is down") else tree_kids(path)
  r <- call_handler(lazy_state(kids), list(paths = list(list("bad"), list("a")), generation = 1))
  expect_equal(r$status, 200L)
  expect_equal(r$body$nodes[[1]]$error, "database is down")
  expect_null(r$body$nodes[[1]]$children)
  expect_length(r$body$nodes[[2]]$children, 2)
})

test_that("a children function returning the wrong thing is reported, not crashed on", {
  for (bad in list(1:3, "x", list("unnamed"), stats::setNames(list(1), ""))) {
    r <- call_handler(lazy_state(function(path) bad), list(paths = list(list()), generation = 1))
    expect_equal(r$status, 200L)
    expect_match(r$body$nodes[[1]]$error, "named list")
  }
  ## NULL and an empty list are a node with no children.
  for (empty in list(NULL, list())) {
    r <- call_handler(lazy_state(function(path) empty), list(paths = list(list()), generation = 1))
    expect_length(r$body$nodes[[1]]$children, 0)
  }
})

test_that("a request for a rebuilt tree is refused as stale", {
  r <- call_handler(lazy_state(tree_kids, generation = 3L), list(paths = list(list()), generation = 2))
  expect_equal(r$status, 409L)
})

test_that("malformed requests are rejected with 400, never evaluated", {
  st <- lazy_state(function(path) stop("must not be called"))
  bad_bodies <- list(
    "not json",
    "",
    list(nothing = TRUE, generation = 1),
    list(paths = "a", generation = 1),
    list(paths = list(list(1, 2)), generation = 1),
    list(paths = list(list(list("nested"))), generation = 1),
    list(search = list("a", "b"), generation = 1),
    list(search = 5, generation = 1)
  )
  for (b in bad_bodies) {
    r <- call_handler(st, b)
    expect_equal(r$status, 400L, info = paste(deparse(b), collapse = ""))
  }
  expect_equal(call_handler(st, list(paths = list(list()), generation = 1), method = "GET")$status, 400L)
  ## A request asking for too many paths at once.
  many <- rep(list(list("a")), 1001)
  expect_equal(call_handler(st, list(paths = many, generation = 1))$status, 400L)
})

test_that("a request before the tree is rendered is answered 503", {
  st <- new.env(parent = emptyenv()); st$spec <- NULL; st$generation <- 0L
  res <- lazyRequestHandler(st, fake_request(list(paths = list(list()), generation = 0)))
  expect_equal(res$status, 503L)
})

test_that("search walks the whole tree by default, matching names case-insensitively", {
  r <- call_handler(lazy_state(tree_kids), list(search = "A2", generation = 1))
  expect_equal(r$status, 200L)
  expect_equal(lapply(r$body$matches, unlist), list(c("a", "a2")))
  r <- call_handler(lazy_state(tree_kids), list(search = "deep", generation = 1))
  expect_equal(lapply(r$body$matches, unlist), list(c("a", "a2", "deep")))
  ## Inline children are searched without asking the children function again.
  r <- call_handler(lazy_state(tree_kids), list(search = "c2x", generation = 1))
  expect_equal(lapply(r$body$matches, unlist), list(c("c", "c2", "c2x")))
  ## An empty search matches nothing.
  expect_length(call_handler(lazy_state(tree_kids), list(search = "", generation = 1))$body$matches, 0)
})

test_that("search text is matched literally, not as a regular expression", {
  kids <- function(path) if (!length(path)) stats::setNames(list("", "", ""), c("a.b", "axb", "(x")) else list()
  r <- call_handler(lazy_state(kids), list(search = ".", generation = 1))
  expect_equal(lapply(r$body$matches, unlist), list("a.b"))
  r <- call_handler(lazy_state(kids), list(search = "(", generation = 1))
  expect_equal(lapply(r$body$matches, unlist), list("(x"))
})

test_that("a search function is used instead of walking the tree", {
  walked <- FALSE
  kids <- function(path) { walked <<- TRUE; tree_kids(path) }
  srch <- function(text) list(c("a", "a2", "deep"), "b")
  r <- call_handler(lazy_state(kids, search = srch), list(search = "anything", generation = 1))
  expect_equal(lapply(r$body$matches, unlist), list(c("a", "a2", "deep"), "b"))
  expect_false(walked)
  ## A character vector of single-name paths is accepted; NULL means none.
  r <- call_handler(lazy_state(kids, search = function(text) c("a", "b")), list(search = "x", generation = 1))
  expect_equal(lapply(r$body$matches, unlist), list("a", "b"))
  r <- call_handler(lazy_state(kids, search = function(text) NULL), list(search = "x", generation = 1))
  expect_length(r$body$matches, 0)
  ## An error in it is reported.
  r <- call_handler(lazy_state(kids, search = function(text) stop("search index offline")),
                    list(search = "x", generation = 1))
  expect_equal(r$status, 500L)
  expect_match(r$body$error, "search index offline")
})

test_that("a default search on an endless tree stops instead of hanging", {
  endless <- function(path) list(x = structure("", stchildren = TRUE), y = structure("", stchildren = TRUE))
  st <- lazy_state(endless)
  ## lazySearch is given a small budget directly to keep the test fast.
  expect_error(lazySearch(st$spec, "nomatch", max_nodes = 500L), "too large or slow to search")
})

test_that("normaliseLazySpec accepts a function or a list and rejects anything else", {
  expect_true(is.function(normaliseLazySpec(function(path) list())$children))
  spec <- normaliseLazySpec(list(children = function(path) list(), search = function(t) list()))
  expect_true(is.function(spec$search))
  expect_error(normaliseLazySpec(list(a = 1)), "must return a function")
  expect_error(normaliseLazySpec("nope"), "must return a function")
  expect_error(normaliseLazySpec(list(children = function(p) list(), search = "no")), "`search` must be a function")
})

## --- Regressions from adversarial testing --------------------------------

test_that("the default search is linear and bounded, so it cannot freeze the app", {
  wide <- function(n) function(path) {
    if (length(path)) return(list())
    stats::setNames(rep(list(structure("", stchildren = TRUE)), n), paste0("n", seq_len(n)))
  }
  ## 99,999 branches used to take about a minute and block every session.
  t <- system.time(r <- call_handler(lazy_state(wide(99999)), list(search = "zzz", generation = 1)))
  expect_equal(r$status, 500L)
  expect_match(r$body$error, "too large or slow to search")
  expect_lt(t[["elapsed"]], 5)
  ## The cap counts children-function calls, not just nodes.
  calls <- 0
  deep <- function(path) { calls <<- calls + 1
    if (length(path) >= 8) list() else stats::setNames(rep(list(structure("", stchildren = TRUE)), 10), paste0("k", 1:10)) }
  expect_error(lazySearch(normaliseLazySpec(deep), "zzz", max_calls = 200L), "too large or slow to search")
  expect_lte(calls, 201)
})

test_that("errors from the app's code follow shiny.sanitize.errors", {
  kids <- function(path) if (identical(path, "bad")) stop("SECRET password=hunter2") else list()
  srch <- function(text) stop("SECRET search")
  withr::local_options(shiny.sanitize.errors = TRUE)
  r <- call_handler(lazy_state(kids), list(paths = list(list("bad")), generation = 1))
  expect_false(grepl("SECRET", r$body$nodes[[1]]$error))
  r <- call_handler(lazy_state(kids, search = srch), list(search = "x", generation = 1))
  expect_false(grepl("SECRET", r$body$error))
  ## A safeError() is shown as it is, as Shiny does elsewhere.
  safe <- function(path) stop(shiny::safeError("Shown on purpose"))
  r <- call_handler(lazy_state(safe), list(paths = list(list()), generation = 1))
  expect_equal(r$body$nodes[[1]]$error, "Shown on purpose")
})

test_that("a path the browser was never given is refused without calling the app", {
  asked <- list()
  kids <- function(path) { asked[[length(asked) + 1]] <<- path; list(a = structure("", stchildren = TRUE)) }
  st <- lazy_state(kids, offer = list(character(0)))
  r <- call_handler(st, list(paths = list(list("..", ".."), list("a")), generation = 1))
  expect_equal(r$body$nodes[[1]]$error, "This node is not in the tree.")
  expect_equal(r$body$nodes[[2]]$error, "This node is not in the tree.")
  expect_length(asked, 0)
  ## Once the top level has been sent, "a" can be fetched.
  call_handler(st, list(paths = list(list()), generation = 1))
  r <- call_handler(st, list(paths = list(list("a")), generation = 1))
  expect_null(r$body$nodes[[1]]$error)
  expect_equal(asked, list(character(0), "a"))
  ## Search results are offered too, so their branches can be opened.
  st2 <- lazy_state(kids, search = function(t) list(c("x", "y", "z")), offer = list(character(0)))
  call_handler(st2, list(search = "z", generation = 1))
  expect_true(isOffered(st2, c("x", "y")))
})

test_that("one child that cannot be sent fails its own branch, not the batch", {
  kids <- function(path) {
    if (identical(path, "bad")) list(x = structure("", stfoo = new.env()))
    else list(bad = structure("", stchildren = TRUE), fine = "")
  }
  r <- call_handler(lazy_state(kids), list(paths = list(list(), list("bad")), generation = 1))
  expect_equal(r$status, 200L)
  expect_length(r$body$nodes[[1]]$children, 2)
  expect_match(r$body$nodes[[2]]$error, "could not be sent")
})

test_that("a bare condition from stop() is a failure for that node", {
  kids <- function(path) stop(structure(class = c("mycond", "condition"), list(message = "custom", call = NULL)))
  r <- call_handler(lazy_state(kids), list(paths = list(list()), generation = 1))
  expect_equal(r$status, 200L)
  expect_equal(r$body$nodes[[1]]$error, "custom")
})

test_that("warnings and messages do not stop the children function", {
  kids <- function(path) { warning("careful"); message("note"); list(a = "") }
  st <- lazy_state(kids)
  out <- utils::capture.output(
    res <- lazyRequestHandler(st, fake_request(list(paths = list(list()), generation = 1))), type = "message")
  body <- jsonlite::fromJSON(res$content, simplifyVector = FALSE)
  expect_equal(body$nodes[[1]]$children[[1]]$text, "a")
  expect_true(any(grepl("careful", out)))
  expect_true(any(grepl("note", out)))
})

test_that("errors are also reported in the R console", {
  kids <- function(path) stop("database is down")
  st <- lazy_state(kids)
  out <- utils::capture.output(
    lazyRequestHandler(st, fake_request(list(paths = list(list()), generation = 1))), type = "message")
  expect_true(any(grepl("renderTree\\(lazy = TRUE\\).*database is down", out)))
})

test_that("search results always come back as an array of paths", {
  srch <- function(t) list(c("a", "b"), "c")
  r <- call_handler(lazy_state(function(p) list(), search = srch), list(search = "x", generation = 1))
  expect_equal(r$body$matches, list(list("a", "b"), list("c")))
  ## Named lists lose their names rather than becoming a JSON object.
  r <- call_handler(lazy_state(function(p) list(), search = function(t) split(c("a", "b"), 1:2)),
                    list(search = "x", generation = 1))
  expect_equal(r$body$matches, list(list("a"), list("b")))
  for (bad in list(function(t) data.frame(a = 1), function(t) list(list("b")),
                   function(t) list(c("a", NA)), function(t) list(new.env()))) {
    r <- call_handler(lazy_state(function(p) list(), search = bad), list(search = "x", generation = 1))
    expect_equal(r$status, 500L)
    expect_match(r$body$error, "list of paths")
  }
})

test_that("generation must be the exact number", {
  for (g in list("1", 1.9, TRUE, list(a = 1), "1abc", 4294967297)) {
    r <- call_handler(lazy_state(function(p) list()), list(paths = list(list()), generation = g))
    expect_equal(r$status, 409L, info = deparse(g))
  }
  expect_silent(call_handler(lazy_state(function(p) list()), list(paths = list(list()), generation = "1abc")))
})

test_that("paths must be an array, and children need real names", {
  r <- call_handler(lazy_state(function(p) list()), list(paths = list(a = list("x")), generation = 1))
  expect_equal(r$status, 400L)
  r <- call_handler(lazy_state(function(p) stats::setNames(list("", ""), c("a", NA))),
                    list(paths = list(list()), generation = 1))
  expect_match(r$body$nodes[[1]]$error, "non-empty name")
  r <- call_handler(lazy_state(function(p) data.frame(a = 1:2)), list(paths = list(list()), generation = 1))
  expect_match(r$body$nodes[[1]]$error, "dfToTree")
})

test_that("the children function can read reactive values", {
  ## Through a factory's lazily evaluated argument, through ..., and nested --
  ## all read in the HTTP request, which is outside any reactive context.
  vals <- shiny::reactiveValues(folder = "/data")
  session <- shiny::MockShinySession$new()
  make_dots <- function(...) function(path) list(...)
  make_nested <- function(root) { inner <- function(sep) function(path) stats::setNames(list(""), paste(root, sep)); inner("/") }
  for (f in list(make_dots(leaf = vals$folder), make_nested(vals$folder),
                 function(path) stats::setNames(list(""), vals$folder))) {
    st <- lazy_state(f); st$session <- session
    r <- call_handler(st, list(paths = list(list()), generation = 1))
    expect_null(r$body$nodes[[1]]$error)
    expect_length(r$body$nodes[[1]]$children, 1)
  }
})

test_that("siblings with the same name are told apart by their data (function(path, node))", {
  seen <- list()
  kids <- function(path, node) {
    if (!length(path)) {
      return(list(dup = structure("", stchildren = TRUE, stid = 1),
                  dup = structure("", stchildren = TRUE, stid = 2)))
    }
    seen[[length(seen) + 1]] <<- node
    stats::setNames(list(""), paste0("child of ", node$stid))
  }
  st <- lazy_state(kids, offer = list(character(0)))
  top <- call_handler(st, list(paths = list(list()), generation = 1))$body$nodes[[1]]$children
  ## Each node carries its path as stpath, and its own data.
  expect_equal(unlist(top[[1]]$data$stpath), "dup")
  expect_equal(c(top[[1]]$data$stid, top[[2]]$data$stid), c(1, 2))
  r <- call_handler(st, list(paths = list(list("dup"), list("dup")),
                             data = list(list(stid = 1), list(stid = 2)), generation = 1))
  expect_equal(vapply(r$body$nodes, function(n) n$children[[1]]$text, ""),
               c("child of 1", "child of 2"))
  ## Only st* data reaches the function.
  r <- call_handler(st, list(paths = list(list("dup")),
                             data = list(list(stid = 1, other = "x")), generation = 1))
  expect_equal(names(seen[[length(seen)]]), "stid")
  ## A one-argument function still works.
  r <- call_handler(lazy_state(function(path) list(a = "")),
                    list(paths = list(list()), data = list(list(stid = 1)), generation = 1))
  expect_equal(r$body$nodes[[1]]$children[[1]]$text, "a")
  ## data must line up with paths.
  r <- call_handler(st, list(paths = list(list("dup")), data = list(), generation = 1))
  expect_equal(r$status, 400L)
})

test_that("stchildren must be TRUE or FALSE", {
  expect_warning(
    kids <- lazyChildren(normaliseLazySpec(function(p) list(x = structure("", stchildren = "yes"))), character(0)),
    "should be TRUE or FALSE")
  expect_null(kids[[1]]$children)
  expect_silent(lazyChildren(normaliseLazySpec(function(p) list(x = structure("", stchildren = FALSE))), character(0)))
})

test_that("renderTree() without lazy says so when given a function", {
  rf <- renderTree(function(path) list(a = ""))
  fake_session <- list(sendInputMessage = function(...) NULL)
  expect_error(rf(fake_session, "tree"), "lazy = TRUE")
})

test_that("updateTree() and expandTree() refuse a lazy tree", {
  session <- shiny::MockShinySession$new()
  markLazyOutput(session, session$ns("tree"))
  expect_error(updateTree(session, "tree", list(a = "")), "renderTree\\(lazy = TRUE\\)")
  expect_error(expandTree(session, "tree"), "whole tree")
  ## Other trees in the same session are unaffected.
  sent <- NULL
  session$sendInputMessage <- function(id, msg) sent <<- msg
  expect_silent(updateTree(session, "other", list(a = "")))
  expect_silent(collapseTree(session, "tree"))
})


## --- Regressions from the second round of adversarial testing --------------

test_that("a leaf is never offered, so it cannot be asked for its children", {
  ## e.g. a symbolic link the app shows as a leaf, which list.dirs() would follow.
  asked <- list()
  kids <- function(path) {
    asked[[length(asked) + 1]] <<- path
    if (!length(path)) list(etclink = "", dir = structure("", stchildren = TRUE), inline = list(x = "")) else list()
  }
  st <- lazy_state(kids, offer = list(character(0)))
  call_handler(st, list(paths = list(list()), generation = 1))
  r <- call_handler(st, list(paths = list(list("etclink"), list("dir"), list("inline", "x")), generation = 1))
  expect_equal(r$body$nodes[[1]]$error, "This node is not in the tree.")
  expect_null(r$body$nodes[[2]]$error)
  expect_equal(r$body$nodes[[3]]$error, "This node is not in the tree.")
  expect_equal(asked, list(character(0), "dir"))
  ## A branch given inline is offered, and so can be asked for again.
  expect_true(isOffered(st, "inline"))
  ## A search match is offered only through its ancestors, not itself.
  st2 <- lazy_state(kids, search = function(t) list(c("dir", "leafy")), offer = list(character(0)))
  call_handler(st2, list(search = "x", generation = 1))
  expect_true(isOffered(st2, "dir"))
  expect_false(isOffered(st2, c("dir", "leafy")))
})

test_that("only an argument called `node` receives the node's data", {
  spec <- function(f) normaliseLazySpec(f)
  got <- NULL
  expect_silent(lazyChildren(spec(function(path, n = 10) { got <<- n; list() }), "x", list(stid = 1)))
  expect_equal(got, 10)
  lazyChildren(spec(function(path, extra = 1) { got <<- extra; list() }), "x", list(stid = 1))
  expect_equal(got, 1)
  lazyChildren(spec(function(path, node) { got <<- node; list() }), "x", list(stid = 1))
  expect_equal(got, list(stid = 1))
  lazyChildren(spec(function(node, path) { got <<- list(node, path); list() }), "x", list(stid = 1))
  expect_equal(got, list(list(stid = 1), "x"))
  lazyChildren(spec(function(...) { got <<- list(...); list() }), "x", list(stid = 1))
  expect_equal(got, list("x"))
})

test_that("the default search passes each branch's own data, telling duplicates apart", {
  kids <- function(path, node) {
    if (!length(path)) {
      return(list(dup = structure("", stchildren = TRUE, stid = 1),
                  dup = structure("", stchildren = TRUE, stid = 2)))
    }
    stats::setNames(list(""), paste0("only_in_dup", node$stid))
  }
  m <- lazySearch(normaliseLazySpec(kids), "only")
  expect_equal(m, list(c("dup", "only_in_dup1"), c("dup", "only_in_dup2")))
})

test_that("a failing branch is skipped by the default search", {
  kids <- function(path) {
    if (!length(path)) return(list(bad = structure("", stchildren = TRUE), good = structure("", stchildren = TRUE)))
    if (identical(path, "bad")) stop("cannot list this one")
    list(leafx = "")
  }
  expect_equal(lazySearch(normaliseLazySpec(kids), "leafx"), list(c("good", "leafx")))
  ## With nothing found, the failure is reported rather than "no matches".
  only_bad <- function(path) if (!length(path)) list(bad = structure("", stchildren = TRUE)) else stop("no")
  expect_error(lazySearch(normaliseLazySpec(only_bad), "x"), "could not be searched")
})

test_that("a user's own stpath is replaced, with a warning", {
  expect_warning(
    kids <- lazyChildren(normaliseLazySpec(function(p) list(mine = structure("", stpath = "user value"))), character(0)),
    "stpath attribute")
  expect_equal(unclass(kids[[1]]$data$stpath), "mine")
})

## --- Regressions from the server re-test ------------------------------------

test_that("informational signals do not fail the children function", {
  ## dplyr's summarise() with several groups signals one nothing handles.
  sig <- function(path) {
    signalCondition(structure(class = c("dplyr_regroup", "condition"), list(message = "regroup", call = NULL)))
    rlang::signal("progress", class = "my_progress")
    list(a = "")
  }
  r <- call_handler(lazy_state(sig), list(paths = list(list()), generation = 1))
  expect_null(r$body$nodes[[1]]$error)
  expect_equal(r$body$nodes[[1]]$children[[1]]$text, "a")
  r <- call_handler(lazy_state(sig), list(search = "a", generation = 1))
  expect_equal(r$status, 200L)
})

test_that("repeating a path in one request does not repeat the work", {
  calls <- 0
  big <- function(path) { calls <<- calls + 1; stats::setNames(rep(list(""), 1000), paste0("k", 1:1000)) }
  st <- lazy_state(big)
  t <- system.time(r <- call_handler(st, list(paths = rep(list(list()), 1000), generation = 1)))
  expect_equal(calls, 1)
  expect_lt(t[["elapsed"]], 10)
  ## The response is capped; past the cap a node says so.
  last <- r$body$nodes[[length(r$body$nodes)]]
  expect_match(last$error, "Too much was asked for")
  expect_null(r$body$nodes[[1]]$error)
})

test_that("the children function only ever sees node data the server gave out", {
  db <- list(`1` = list(public_row = ""), `2` = list(SECRET_ROW = ""))
  kids <- function(path, node) {
    if (!length(path)) list(public = structure("", stchildren = TRUE, stid = 1)) else db[[as.character(node$stid)]]
  }
  st <- lazy_state(kids, offer = list(character(0)))
  call_handler(st, list(paths = list(list()), generation = 1))
  r <- call_handler(st, list(paths = list(list("public")), data = list(list(stid = 2)), generation = 1))
  expect_equal(r$body$nodes[[1]]$children[[1]]$text, "public_row")
  ## Same-named siblings still get their own, as the server sent them.
  dups <- function(path, node) {
    if (!length(path)) return(list(dup = structure("", stchildren = TRUE, stid = 1),
                                   dup = structure("", stchildren = TRUE, stid = 2)))
    stats::setNames(list(""), paste("child of", node$stid))
  }
  st <- lazy_state(dups, offer = list(character(0)))
  call_handler(st, list(paths = list(list()), generation = 1))
  r <- call_handler(st, list(paths = list(list("dup"), list("dup"), list("dup")),
                             data = list(list(stid = 1), list(stid = 2), list(stid = 99)), generation = 1))
  expect_equal(vapply(r$body$nodes, function(n) n$children[[1]]$text, ""),
               c("child of 1", "child of 2", "child of 1"))
})

test_that("a second argument not called node is left alone, and ... forwards only path", {
  r <- call_handler(lazy_state(function(path, root = "SAFE") stats::setNames(list(""), root)),
                    list(paths = list(list()), data = list(list(stroot = "/etc")), generation = 1))
  expect_equal(r$body$nodes[[1]]$children[[1]]$text, "SAFE")
  inner <- function(path) list(a = "")
  r <- call_handler(lazy_state(function(...) inner(...)), list(paths = list(list()), generation = 1))
  expect_equal(r$body$nodes[[1]]$children[[1]]$text, "a")
})

test_that("path keys cannot collide", {
  st <- lazy_state(function(p) list(), offer = list(character(0), c("a", "b")))
  r <- call_handler(st, list(paths = list(list("a\u0001b"), list("")), generation = 1))
  expect_equal(vapply(r$body$nodes, `[[`, "", "error"), rep("This node is not in the tree.", 2))
  expect_false(identical(pathKey(character(0)), pathKey("")))
})

test_that("the default search has a time budget", {
  slow <- function(path) { Sys.sleep(0.01); stats::setNames(rep(list(structure("", stchildren = TRUE)), 10), paste0("k", 1:10)) }
  t <- system.time(expect_error(lazySearch(normaliseLazySpec(slow), "zzz", max_seconds = 0.5), "too large or slow"))
  expect_lt(t[["elapsed"]], 3)
})

test_that("validate() messages are shown and req() is silent, as in Shiny", {
  withr::local_options(shiny.sanitize.errors = TRUE)
  r <- call_handler(lazy_state(function(p) shiny::validate(shiny::need(FALSE, "Please choose a folder first"))),
                    list(paths = list(list()), generation = 1))
  expect_equal(r$body$nodes[[1]]$error, "Please choose a folder first")
  st <- lazy_state(function(p) shiny::req(FALSE))
  out <- utils::capture.output(lazyRequestHandler(st, fake_request(list(paths = list(list()), generation = 1))), type = "message")
  expect_length(out, 0)
})

test_that("names that are not valid UTF-8 cannot break the response", {
  bad <- rawToChar(as.raw(c(0x61, 0xff, 0x62)))
  st <- lazy_state(function(p) stats::setNames(list(""), bad))
  res <- suppressMessages(lazyRequestHandler(st, fake_request(list(paths = list(list()), generation = 1))))
  expect_true(validUTF8(res$content))
  expect_equal(jsonlite::fromJSON(res$content, simplifyVector = FALSE)$nodes[[1]]$children[[1]]$text, "a?b")
})

test_that("an error with no message is reported by its class", {
  st <- lazy_state(function(p) stop(structure(class = c("empty_cond", "error", "condition"), list(message = "", call = NULL))))
  out <- utils::capture.output(lazyRequestHandler(st, fake_request(list(paths = list(list()), generation = 1))), type = "message")
  expect_true(any(grepl("renderTree\\(lazy = TRUE\\): empty_cond", out)))
})

test_that("a children function must take path", {
  expect_error(normaliseLazySpec(function(node) list()), "must take the node's `path`")
  expect_error(normaliseLazySpec(function() list()), "must take the node's `path`")
  expect_silent(normaliseLazySpec(function(path) list()))
  expect_silent(normaliseLazySpec(function(path, node) list()))
  expect_silent(normaliseLazySpec(function(...) list()))
})

## --- Regressions from the third server pass ---------------------------------

test_that("junk data per repeat does not defeat the reuse of answers", {
  calls <- 0
  kids <- function(path) { calls <<- calls + 1; list(a = "") }
  junk <- lapply(1:500, function(i) list(stjunk = i))
  r <- call_handler(lazy_state(kids), list(paths = rep(list(list()), 500), data = junk, generation = 1))
  expect_equal(calls, 1)
  expect_null(r$body$nodes[[500]]$error)
})

test_that("distinct paths per request are capped", {
  calls <- 0
  kids <- function(path) { calls <<- calls + 1; list() }
  offer <- c(list(character(0)), lapply(1:300, function(i) paste0("n", i)))
  r <- call_handler(lazy_state(kids, offer = offer),
                    list(paths = lapply(1:300, function(i) list(paste0("n", i))), generation = 1))
  expect_lte(calls, 200)
  expect_match(r$body$nodes[[300]]$error, "Too much was asked for")
})

test_that("data that changes on every fetch does not pile up", {
  kids <- function(path) if (!length(path)) list(k = structure("", stchildren = TRUE, stts = runif(1))) else list()
  st <- lazy_state(kids, offer = list(character(0)))
  for (i in 1:20) call_handler(st, list(paths = list(list()), generation = 1))
  expect_length(get0(pathKey("k"), envir = st$offered), 1)
})

test_that("an earlier handled signal is not blamed for a later abort", {
  kids <- function(path) {
    withCallingHandlers(rlang::signal("harmless note", class = "my_progress"), my_progress = function(c) NULL)
    invokeRestart("abort")
  }
  r <- call_handler(lazy_state(kids), list(paths = list(list()), generation = 1))
  expect_false(grepl("harmless", r$body$nodes[[1]]$error))
})

test_that("names that differ only in invalid bytes stay apart, and come back intact", {
  a <- rawToChar(as.raw(c(0x61, 0xff, 0x62)))
  b <- rawToChar(as.raw(c(0x61, 0xfe, 0x62)))
  got <- list()
  kids <- function(path) {
    got[[length(got) + 1]] <<- path
    if (!length(path)) stats::setNames(list(structure("", stchildren = TRUE), structure("", stchildren = TRUE)), c(a, b))
    else list(x = "")
  }
  st <- lazy_state(kids, offer = list(character(0)))
  top <- call_handler(st, list(paths = list(list()), generation = 1))$body$nodes[[1]]$children
  p1 <- unlist(top[[1]]$data$stpath); p2 <- unlist(top[[2]]$data$stpath)
  expect_false(identical(p1, p2))
  expect_equal(c(top[[1]]$text, top[[2]]$text), c("a?b", "a?b"))
  call_handler(st, list(paths = list(as.list(p2)), generation = 1))
  expect_identical(got[[2]], b)
})

test_that("invalid UTF-8 in attributes, errors and search cannot break the response", {
  bad <- rawToChar(as.raw(c(0x61, 0xff, 0x62)))
  for (f in list(function(p) list(n = structure("", sttooltip = bad, stfoo = bad)),
                 function(p) stop("bad bytes: ", bad))) {
    st <- lazy_state(f)
    res <- suppressMessages(utils::capture.output(
      out <- lazyRequestHandler(st, fake_request(list(paths = list(list()), generation = 1))), type = "message"))
    expect_true(validUTF8(out$content))
  }
  r <- call_handler(lazy_state(function(p) if (!length(p)) stats::setNames(list(""), bad) else list()),
                    list(search = "a", generation = 1))
  expect_equal(r$status, 200L)
  expect_length(r$body$matches, 1)
})

test_that("req(FALSE) is silent: no error for the browser to report", {
  r <- call_handler(lazy_state(function(p) shiny::req(FALSE)), list(paths = list(list()), generation = 1))
  expect_true(isTRUE(r$body$nodes[[1]]$silent))
  expect_equal(r$body$nodes[[1]]$error, "")
})

test_that("stids are sent at full precision, so close ones stay distinct", {
  kids <- function(path, node) {
    if (!length(path)) return(list(dup = structure("", stchildren = TRUE, stid = 1.00001),
                                   dup = structure("", stchildren = TRUE, stid = 1.00002)))
    stats::setNames(list(""), format(node$stid, digits = 10))
  }
  st <- lazy_state(kids, offer = list(character(0)))
  top <- call_handler(st, list(paths = list(list()), generation = 1))$body$nodes[[1]]$children
  expect_equal(top[[2]]$data$stid, 1.00002)
  r <- call_handler(st, list(paths = list(list("dup")), data = list(list(stid = 1.00002)), generation = 1))
  expect_equal(r$body$nodes[[1]]$children[[1]]$text, "1.00002")
})

test_that("options(warn = 2) still turns a warning into an error", {
  kids <- function(p) { op <- options(warn = 2); on.exit(options(op)); warning("now an error"); list(a = "") }
  r <- call_handler(lazy_state(kids), list(paths = list(list()), generation = 1))
  expect_match(r$body$nodes[[1]]$error, "converted from warning")
})
