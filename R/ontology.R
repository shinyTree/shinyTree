#' Show an ontology as a tree
#'
#' Reads an ontology from an OBO (\code{.obo}) or OWL (\code{.owl}, RDF/XML)
#' file and returns a children function for \code{\link{renderTree}(lazy =
#' TRUE)}, so even a large ontology such as the Gene Ontology is shown a level
#' at a time.
#'
#' The hierarchy is formed by \code{is_a} (in OWL, \code{rdfs:subClassOf}
#' between named classes). Each node is labelled with the term's name and
#' carries its identifier as \code{stid} and its definition, if any, as
#' \code{sttooltip}, so it is shown on hover.
#'
#' \describe{
#'   \item{Several parents}{A term with more than one parent is shown under
#'     each of them, with the same \code{stid} in every place, and search
#'     finds it in each.}
#'   \item{Obsolete terms}{Terms marked obsolete (\code{is_obsolete: true}, or
#'     \code{owl:deprecated}) have no parent, so they are gathered under one
#'     extra top-level node, \code{obsolete}, and shown greyed out
#'     (\code{stdisabled}). With \code{obsolete = NULL} they are left out, and
#'     a live term whose only parents are obsolete is shown at the top level.}
#'   \item{Terms left out}{OWL classes with no label, and OBO stanzas with no
#'     name and no parent, are placeholders -- typically left behind when
#'     terms are merged or replaced -- and are not shown.}
#'   \item{Parents outside the file}{Only terms in the file are shown. A term
#'     whose parents are all outside it -- in an ontology it imports, say --
#'     appears at the top level.}
#'   \item{OBO and OWL}{The OWL release of an ontology usually includes every
#'     class it imports from other ontologies, which its OBO release leaves
#'     out, so the two can have different top levels. The OBO file usually
#'     gives the tidier tree.}
#'   \item{Cycles}{A term caught in a cycle of \code{is_a} -- A is_a B, B is_a
#'     A -- with no way up to a root is shown at the top level, so nothing in
#'     the file is lost.}
#'   \item{Names}{Siblings can share a name; they are told apart by
#'     \code{stid}, so the function takes \code{node} as well as \code{path}.
#'     Names are shown as they are in the file, as HTML (see
#'     \code{\link{renderTree}}).}
#'   \item{Size}{The Gene Ontology's 48,000 terms are read in a few seconds,
#'     and each branch is fetched only when opened. A branch with thousands of
#'     children, such as its obsolete terms, takes a few seconds to open.}
#' }
#'
#' @param file Path to an \code{.obo} or \code{.owl} file.
#' @param format \code{"obo"} or \code{"owl"}; by default guessed from the
#'   file's extension.
#' @param obsolete Label of the top-level node holding the obsolete terms, or
#'   \code{NULL} to leave them out.
#' @return A list for \code{renderTree(lazy = TRUE)}: \code{children}, the
#'   function giving a node's children, and \code{search}, which finds terms
#'   whose name or identifier contains the search text and returns the path
#'   to each place a matching term appears (a term with several parents
#'   appears in several). It also has an attribute \code{"ontology"}: a data
#'   frame of the terms, with columns \code{id}, \code{name},
#'   \code{definition} and \code{obsolete}, and a list column \code{parents}.
#' @examples
#' \dontrun{
#' library(shiny)
#' go <- ontologyToTree("go-basic.obo")
#' ui <- fluidPage(shinyTree("tree", search = TRUE))
#' server <- function(input, output, session) {
#'   output$tree <- renderTree(lazy = TRUE, go)
#' }
#' shinyApp(ui, server)
#'
#' # The 28-ontology example browses the ontology from shinyTree issue #33:
#' runApp(system.file("examples/28-ontology", package = "shinyTree"))
#' }
#' @seealso \code{\link{renderTree}}, whose \emph{Lazy loading} section
#'   describes the children function
#' @export
ontologyToTree <- function(file, format = NULL, obsolete = "obsolete") {
  if (!is.character(file) || length(file) != 1 || is.na(file) || !file.exists(file) ||
      dir.exists(file)) {
    stop("`file` must be the path of an existing .obo or .owl file.", call. = FALSE)
  }
  if (!is.null(obsolete) &&
      !(is.character(obsolete) && length(obsolete) == 1 && !is.na(obsolete) && nzchar(obsolete))) {
    stop("`obsolete` must be the label for the node holding obsolete terms, ",
         "e.g. \"obsolete\", or NULL to leave them out.", call. = FALSE)
  }
  if (is.null(format)) {
    ext <- tolower(tools::file_ext(file))
    format <- switch(ext, obo = "obo", owl = "owl", rdf = "owl", xml = "owl",
                     stop("Cannot tell the ontology's format from the file name; give ",
                          "format = \"obo\" or \"owl\".", call. = FALSE))
  }
  format <- match.arg(tolower(format), c("obo", "owl"))
  terms <- if (format == "obo") readObo(file) else readOwl(file)
  ontologyChildren(terms, obsolete)
}

#' Build the children function from a table of terms
#' @noRd
ontologyChildren <- function(terms, obsolete = "obsolete") {
  terms <- terms[!is.na(terms$id) & nzchar(terms$id), , drop = FALSE]
  ## A term given twice keeps its first name and definition, but the parents
  ## of every copy, so none of its place in the hierarchy is lost.
  dup <- duplicated(terms$id)
  if (any(dup)) {
    allParents <- tapply(seq_len(nrow(terms)), terms$id, function(i) unique(unlist(terms$parents[i])))
    terms <- terms[!dup, , drop = FALSE]
    terms$parents <- I(unname(allParents[terms$id]))
    warning(sprintf("%d term id%s appear more than once in the file; their parents were combined.",
                    sum(dup), if (sum(dup) == 1) "" else "s"), call. = FALSE)
  }
  if (!nrow(terms)) {
    stop("No terms were found in the ontology file.", call. = FALSE)
  }
  ## Parents that are not terms of the ontology (e.g. imported, undefined)
  ## are dropped, so every term reaches a root.
  ## Matched once for all parents together: %in% per term rebuilt a lookup of
  ## every id each time, which took 20 s for the Gene Ontology.
  flat <- unlist(terms$parents)
  owner <- rep(seq_len(nrow(terms)), lengths(terms$parents))
  keep <- flat %in% terms$id
  kept <- split(flat[keep], factor(owner[keep], levels = seq_len(nrow(terms))))
  ## A term is never its own parent.
  terms$parents <- I(unname(Map(function(p, id) setdiff(unique(p), id), kept, terms$id)))
  ## With obsolete terms left out, a parent that is obsolete is no parent: a
  ## live term whose only parents are obsolete becomes a root, rather than
  ## being unreachable.
  if (is.null(obsolete) && any(terms$obsolete)) {
    gone_ids <- terms$id[terms$obsolete]
    terms$parents <- I(lapply(terms$parents, function(p) p[!p %in% gone_ids]))
  }
  live <- !terms$obsolete
  ## A term caught in a cycle of is_a with no way up to a root -- A is_a B,
  ## B is_a A -- would appear nowhere in the tree. Find the live terms that
  ## cannot reach a root or an obsolete term, and show those that start such a
  ## cycle at the top level, so nothing in the file is lost.
  terms$parents <- I(breakCycles(terms$id, terms$parents, terms$obsolete))
  ## Children of each term, in name order; the roots are live terms with no
  ## parent.
  byName <- order(tolower(terms$name), terms$id)
  child_of <- split(terms$id[byName][rep(seq_along(byName), lengths(terms$parents[byName]))],
                    unlist(terms$parents[byName]))
  roots <- terms$id[byName][live[byName] & lengths(terms$parents[byName]) == 0]
  gone <- terms$id[byName][!live[byName]]
  row <- stats::setNames(seq_len(nrow(terms)), terms$id)
  obsoleteId <- "\u0002obsolete"
  ## Worked out once, by position, for every term: looking each child up by
  ## name in a 48,000-entry list, per child, took seconds for a big branch.
  hasKids <- terms$id %in% names(child_of)
  tooltip <- ifelse(!is.na(terms$definition) & nzchar(terms$definition),
                    paste0(terms$id, ": ", terms$definition), terms$id)

  nodes <- function(ids) {
    i <- match(ids, terms$id)
    out <- lapply(seq_along(i), function(k) {
      j <- i[k]
      node <- structure("", stid = terms$id[j], sttooltip = tooltip[j])
      if (hasKids[j]) attr(node, "stchildren") <- TRUE
      if (terms$obsolete[j]) attr(node, "stdisabled") <- TRUE
      node
    })
    stats::setNames(out, terms$name[i])
  }

  children <- function(path, node) {
    if (!length(path)) {
      top <- nodes(roots)
      if (!is.null(obsolete) && length(gone)) {
        ## Appended rather than assigned by name, so a real root that happens
        ## to have the same name is kept beside it, not replaced.
        group <- structure("", stid = obsoleteId, stchildren = TRUE,
                           sttooltip = sprintf("%d obsolete terms", length(gone)))
        top <- c(top, stats::setNames(list(group), obsolete))
      }
      return(top)
    }
    id <- node$stid
    if (is.null(id)) return(list())
    if (identical(id, obsoleteId)) return(nodes(gone))
    kids <- child_of[[id]]
    if (!length(kids)) list() else nodes(kids)
  }
  ## Search by name or id. A term can sit in several places -- one per path
  ## from a root -- and each is a match; the number of paths can grow fast in
  ## a deep, many-parented ontology, so at most `limit` are returned.
  parentsOf <- stats::setNames(terms$parents, terms$id)
  search <- function(text, limit = 1000L) {
    if (!is.character(text) || length(text) != 1 || is.na(text) || !nzchar(text)) {
      return(list())
    }
    needle <- tolower(text)
    hit <- terms$id[grepl(needle, tolower(terms$name), fixed = TRUE) |
                      grepl(needle, tolower(terms$id), fixed = TRUE)]
    out <- vector("list", 0)
    budget <- new.env(parent = emptyenv())
    budget$n <- 200000L
    for (id in hit) {
      for (p in pathsTo(id, parentsOf, terms, row, obsolete, limit - length(out), budget)) {
        out[[length(out) + 1]] <- p
        if (length(out) >= limit) return(out)
      }
    }
    out
  }
  result <- list(children = children, search = search)
  attr(result, "ontology") <- terms
  result
}

#' Make every term reachable from the top of the tree
#'
#' Marks, by walking down from the roots, every term the tree will show; any
#' live term left unmarked sits only in a cycle (or below one). For each such
#' group, one term -- the first by id -- loses its parents and becomes a root,
#' which makes the whole group reachable. Returns the new parents.
#' @noRd
breakCycles <- function(ids, parents, obsolete) {
  n <- length(ids)
  ## All parent links at once, by position: looking each term's parents up by
  ## name took 20 s on the Gene Ontology.
  flat <- match(unlist(parents, use.names = FALSE), ids)
  owner <- rep(seq_len(n), lengths(parents))
  nParents <- lengths(parents)
  kids <- split(owner, factor(flat, levels = seq_len(n)))
  reached <- logical(n)
  ## Level by level from a set of start terms: each step marks all their
  ## unvisited children at once, so the whole walk is linear in the number of
  ## parent links -- a node-by-node walk with a growing stack took 20 s on the
  ## Gene Ontology.
  spread <- function(frontier) {
    reached[frontier] <<- TRUE
    while (length(frontier)) {
      nxt <- unique(unlist(kids[frontier], use.names = FALSE))
      nxt <- nxt[!reached[nxt]]
      reached[nxt] <<- TRUE
      frontier <- nxt
    }
  }
  spread(which(nParents == 0 | obsolete))
  while (any(!reached)) {
    first <- which(!reached)[order(ids[!reached])][1]
    parents[[first]] <- character(0)
    spread(first)
  }
  parents
}

#' Paths from the top of the tree down to a term
#'
#' Depth-first from the term up through its parents, so whole paths are found
#' one at a time and the search stops as soon as it has `limit` of them --
#' breadth-first, a lattice where every term has two parents built 2^depth
#' partial paths before finishing any. A term already on the path is not
#' visited again, so a cycle cannot be followed round. A path ends at a root,
#' or at an obsolete term, which sits under the obsolete node (so a live term
#' whose only parent is obsolete is found there too). `budget` bounds the
#' parents looked at, so even a pathological ontology cannot keep R busy.
#' @noRd
pathsTo <- function(id, parentsOf, terms, row, obsolete, limit, budget) {
  done <- vector("list", 0)
  if (limit <= 0) return(done)
  ## Iterative, with an explicit stack of (term, next parent to try).
  stack_id <- id
  stack_next <- 1L
  onPath <- stats::setNames(TRUE, id)
  repeat {
    depth <- length(stack_id)
    if (!depth) break
    top <- stack_id[depth]
    i <- row[[top]]
    ps <- parentsOf[[top]]
    atEnd <- terms$obsolete[i] || !length(ps)
    if (atEnd && stack_next[depth] == 1L) {
      names <- terms$name[row[rev(stack_id)]]
      if (terms$obsolete[i]) {
        if (!is.null(obsolete)) done[[length(done) + 1]] <- c(obsolete, names)
      } else {
        done[[length(done) + 1]] <- names
      }
      if (length(done) >= limit) break
      stack_next[depth] <- length(ps) + 1L   # nothing further up from here
    }
    if (atEnd || stack_next[depth] > length(ps)) {
      onPath[top] <- FALSE
      stack_id <- stack_id[-depth]
      stack_next <- stack_next[-depth]
      next
    }
    p <- ps[stack_next[depth]]
    stack_next[depth] <- stack_next[depth] + 1L
    budget$n <- budget$n - 1L
    if (budget$n < 0L) {
      stop("This search has too many paths to show; try a longer search text.", call. = FALSE)
    }
    if (isTRUE(onPath[p]) || depth > 1000L) next   # a cycle, or absurdly deep
    onPath[p] <- TRUE
    stack_id <- c(stack_id, p)
    stack_next <- c(stack_next, 1L)
  }
  done
}

descriptionOf <- function(terms, i) {
  def <- terms$definition[i]
  label <- terms$id[i]
  if (!is.na(def) && nzchar(def)) paste0(label, ": ", def) else label
}

#' Read the terms of an OBO file
#'
#' Only [Term] stanzas are read. Each gives an id, a name, its is_a parents,
#' its definition and whether it is obsolete.
#' @noRd
readObo <- function(file) {
  lines <- readLines(file, warn = FALSE, encoding = "UTF-8")
  bad <- !validUTF8(lines)
  if (any(bad)) {
    ## OBO files are UTF-8; show other bytes as "?" rather than failing on
    ## them, and say so once.
    warning(sprintf("%s has %d line%s that are not valid UTF-8; their bad bytes are shown as \"?\".",
                    basename(file), sum(bad), if (sum(bad) == 1) "" else "s"), call. = FALSE)
    lines[bad] <- iconv(lines[bad], "UTF-8", "UTF-8", sub = "?")
  }
  lines <- sub("\r$", "", lines)
  ## Leading spaces are allowed before tags and stanza headers, and a header
  ## may be followed by a comment or written in another case: [term] ! note.
  lines <- sub("^\\s+", "", lines)
  head <- grepl("^\\[", lines)
  stanza <- cumsum(head)
  isTerm <- stanza %in% stanza[head & grepl("^\\[term\\]\\s*(!.*)?$", lines, ignore.case = TRUE)]
  lines <- lines[isTerm & !head]
  stanza <- stanza[isTerm & !head]
  ## "tag: value"; spaces before the colon (name : x) are allowed too.
  hasTag <- grepl("^[^:]+:", lines)
  lines <- lines[hasTag]
  stanza <- stanza[hasTag]
  tag <- trimws(sub("^([^:]+):.*$", "\\1", lines))
  value <- trimws(sub("^[^:]+:\\s*", "", lines))
  ## A reference may be followed by {qualifiers} and a "! comment", in that
  ## order; either may contain "!" or braces of its own, so the qualifier
  ## block is matched first, then the comment.
  ref <- function(x) {
    x <- sub('\\s*\\{(?:[^{}"]|"(?:[^"\\\\]|\\\\.)*")*\\}(?=\\s*(!.*)?$)', "", x, perl = TRUE)
    trimws(sub("\\s*!.*$", "", x))
  }
  one <- function(t) {
    k <- tag == t
    v <- tapply(value[k], stanza[k], `[`, 1)
    v[as.character(ids_stanza)]
  }
  ids_stanza <- unique(stanza[tag == "id"])
  if (!length(ids_stanza)) {
    stop("No terms were found in the ontology file.", call. = FALSE)
  }
  ## An id may carry a trailing "! comment" too.
  id <- ref(unname(one("id")))
  name <- unname(one("name"))
  def <- unname(one("def"))
  def <- ifelse(is.na(def), NA_character_, sub('^"((?:[^"\\\\]|\\\\.)*)".*$', "\\1", def, perl = TRUE))
  ## A definition whose closing quote is missing: drop the opening one and any
  ## [references] after it.
  open <- !is.na(def) & startsWith(def, '"')
  def[open] <- trimws(sub("\\s*\\[[^]]*\\]\\s*$", "", substring(def[open], 2)))
  ## OBO escapes: \n and \t are a newline and a tab, \W a space; any other
  ## \c is c.
  def <- gsub("\\\\n", "\n", def)
  def <- gsub("\\\\t", "\t", def)
  def <- gsub("\\\\W", " ", def)
  def <- gsub('\\\\(.)', "\\1", def)
  ## is_obsolete can carry a {qualifier} or a comment: is_obsolete: true {source=...}
  obs <- ref(unname(one("is_obsolete")))
  isa <- tag == "is_a"
  parents <- split(ref(value[isa]), factor(stanza[isa], levels = ids_stanza))
  parents <- unname(lapply(parents, unique))
  ## A stanza with no name and no parent is a placeholder -- often just an id
  ## and alt_ids left behind by a merge, as in ChEBI -- not a term to show;
  ## the OWL reader likewise leaves out unlabelled classes. A nameless term
  ## that does have a parent is kept, labelled by its id.
  unnamed <- is.na(name) | !nzchar(name)
  keep <- !(unnamed & lengths(parents) == 0)
  data.frame(
    id = id[keep],
    name = ifelse(unnamed, id, name)[keep],
    definition = def[keep],
    obsolete = (!is.na(obs) & tolower(obs) == "true")[keep],
    parents = I(parents[keep]),
    stringsAsFactors = FALSE
  )
}

#' Read the named classes of an OWL (RDF/XML) file
#'
#' Classes are owl:Class elements -- or rdf:Description typed as owl:Class --
#' named by rdf:about or rdf:ID. Their parents are the named classes they are
#' rdfs:subClassOf, whether by reference or nested; restrictions are not part
#' of the hierarchy. Of several labels, an English one (or one with no
#' language) is preferred. Unlabelled classes are left out.
#' @noRd
readOwl <- function(file) {
  if (!requireNamespace("xml2", quietly = TRUE)) {
    stop("Reading an .owl file needs the xml2 package: install.packages(\"xml2\").",
         call. = FALSE)
  }
  doc <- tryCatch(xml2::read_xml(file), error = function(e) {
    stop("Could not read ", basename(file), " as OWL in RDF/XML: ", conditionMessage(e),
         call. = FALSE)
  })
  ns <- c(owl = "http://www.w3.org/2002/07/owl#",
          rdf = "http://www.w3.org/1999/02/22-rdf-syntax-ns#",
          rdfs = "http://www.w3.org/2000/01/rdf-schema#",
          xml = "http://www.w3.org/XML/1998/namespace")
  base <- xml2::xml_attr(xml2::xml_root(doc), "xml:base", ns = ns)
  if (is.na(base)) base <- ""
  cls <- xml2::xml_find_all(doc, paste0(
    "//owl:Class[@rdf:about or @rdf:ID] | ",
    "//rdf:Description[(@rdf:about or @rdf:ID) and ",
    "rdf:type/@rdf:resource = 'http://www.w3.org/2002/07/owl#Class']"), ns)
  ## A nested owl:Class inside rdfs:subClassOf is a parent reference, not a
  ## class definition of its own here; it is counted as a parent below.
  nested <- vapply(cls, function(x) {
    p <- xml2::xml_parent(x)
    identical(xml2::xml_name(p, ns), "rdfs:subClassOf")
  }, logical(1))
  cls <- cls[!nested]
  iri <- function(nodes) {
    about <- xml2::xml_attr(nodes, "rdf:about", ns = ns)
    id <- xml2::xml_attr(nodes, "rdf:ID", ns = ns)
    ifelse(!is.na(about), about, ifelse(!is.na(id), paste0(base, "#", id), NA_character_))
  }
  about <- iri(cls)
  label <- vapply(cls, function(x) {
    labs <- xml2::xml_find_all(x, "rdfs:label", ns)
    if (!length(labs)) return(NA_character_)
    lang <- tolower(xml2::xml_attr(labs, "xml:lang", ns = ns))
    pick <- which(is.na(lang) | lang == "" | lang == "en" | startsWith(lang, "en-"))
    xml2::xml_text(labs[[if (length(pick)) pick[1] else 1]])
  }, character(1))
  ## The definition is IAO_0000115 under whatever prefix the file gives it.
  def <- vapply(cls, function(x) {
    d <- xml2::xml_find_first(x, "*[local-name() = 'IAO_0000115']")
    if (inherits(d, "xml_missing")) NA_character_ else xml2::xml_text(d)
  }, character(1))
  dep <- vapply(cls, function(x) {
    d <- xml2::xml_find_all(x, "owl:deprecated", ns)
    any(tolower(trimws(xml2::xml_text(d))) %in% c("true", "1"))
  }, logical(1))
  parents <- lapply(cls, function(x) {
    byRef <- xml2::xml_attr(xml2::xml_find_all(x, "rdfs:subClassOf[@rdf:resource]", ns), "rdf:resource", ns = ns)
    byNest <- iri(xml2::xml_find_all(x, "rdfs:subClassOf/owl:Class[@rdf:about or @rdf:ID]", ns))
    c(byRef, byNest[!is.na(byNest)])
  })
  ## Only OBO PURLs are shortened -- http://purl.obolibrary.org/obo/GO_0008150
  ## to GO:0008150, as in OBO files. Any other IRI is kept whole: cutting it to
  ## its last part could make different terms share an id (two namespaces
  ## both ending in /70134, say), and the OBO file writes such terms with
  ## their full IRI too.
  short <- function(iri) {
    obo <- grepl("^https?://purl\\.obolibrary\\.org/obo/[A-Za-z][A-Za-z0-9]*_[^/#]+$", iri)
    iri[obo] <- sub("^https?://purl\\.obolibrary\\.org/obo/([A-Za-z][A-Za-z0-9]*)_", "\\1:", iri[obo])
    iri
  }
  keep <- !is.na(about) & nzchar(about) & !is.na(label) & nzchar(label)
  data.frame(
    id = short(about)[keep],
    name = label[keep],
    definition = def[keep],
    obsolete = dep[keep],
    parents = I(lapply(parents[keep], function(p) unique(short(p)))),
    stringsAsFactors = FALSE
  )
}
