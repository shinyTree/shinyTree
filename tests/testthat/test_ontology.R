context("test_ontology")

## ontologyToTree() (#33), on small hand-written files in tests/testthat/ontology
## that cover the awkward cases: several parents, obsolete terms, undefined
## parents, comments and qualifiers on is_a, Typedef stanzas, a term with no
## name, escaped quotes, CRLF line endings, OWL restrictions and unlabelled
## OWL classes.
onto <- function(f) test_path("ontology", f)
node_names <- function(x) names(x)
ids <- function(x) vapply(x, function(n) attr(n, "stid"), "")

test_that("an OBO file becomes a tree: roots, children, several parents", {
  o <- ontologyToTree(onto("tiny.obo"))
  expect_named(o, c("children", "search"))
  top <- o$children(character(0), list())
  expect_equal(node_names(top), c("entity", "obsolete"))
  expect_true(isTRUE(attr(top$entity, "stchildren")))
  kids <- o$children("entity", list(stid = "T:0000001"))
  expect_equal(node_names(kids), c("material entity", "tissue part"))
  ## "cell" has two parents and appears under each, with the same stid.
  under_material <- o$children(c("entity", "material entity"), list(stid = "T:0000002"))
  under_tissue <- o$children(c("entity", "tissue part"), list(stid = "T:0000005"))
  expect_true("T:0000003" %in% ids(under_material))
  expect_true("T:0000003" %in% ids(under_tissue))
  ## A leaf has no stchildren.
  cell <- under_material[["cell"]]
  expect_null(attr(cell, "stchildren"))
})

test_that("the OBO reader handles comments, qualifiers, Typedefs, names and quotes", {
  terms <- attr(ontologyToTree(onto("tiny.obo")), "ontology")
  expect_setequal(terms$id, sprintf("T:%07d", 1:6))   # Typedef part_of is not a term
  cell <- terms[terms$id == "T:0000003", ]
  expect_equal(sort(cell$parents[[1]]), c("T:0000002", "T:0000005"))   # {qualifier} and ! comment dropped
  ## An undefined parent is dropped, so the term still reaches a root.
  expect_equal(terms$parents[[which(terms$id == "T:0000005")]], "T:0000001")
  ## A term with no name is labelled by its id.
  expect_equal(terms$name[terms$id == "T:0000006"], "T:0000006")
  ## Escaped quotes in a definition are unescaped; the [refs] are dropped.
  expect_equal(terms$definition[terms$id == "T:0000001"], 'Anything at all, with "quotes" inside.')
  expect_true(terms$obsolete[terms$id == "T:0000004"])
  ## The definition is the hover text, after the id.
  top <- ontologyToTree(onto("tiny.obo"))$children(character(0), list())
  expect_match(attr(top$entity, "sttooltip"), '^T:0000001: Anything at all')
})

test_that("obsolete terms are greyed out under one 'obsolete' node, or left out", {
  o <- ontologyToTree(onto("tiny.obo"))
  top <- o$children(character(0), list())
  gone <- o$children("obsolete", list(stid = attr(top$obsolete, "stid")))
  expect_equal(node_names(gone), "old cell")
  expect_true(isTRUE(attr(gone[[1]], "stdisabled")))
  ## Renamed, or left out.
  expect_true("Deprecated" %in% names(ontologyToTree(onto("tiny.obo"), obsolete = "Deprecated")$children(character(0), list())))
  expect_equal(names(ontologyToTree(onto("tiny.obo"), obsolete = NULL)$children(character(0), list())), "entity")
})

test_that("CRLF line endings are read", {
  terms <- attr(ontologyToTree(onto("crlf.obo")), "ontology")
  expect_equal(terms$name, c("windows line endings", "child"))
  expect_equal(terms$parents[[2]], "W:1")
})

test_that("an OWL file gives the same tree as the same ontology in OBO", {
  skip_if_not_installed("xml2")
  owl <- attr(ontologyToTree(onto("tiny.owl")), "ontology")
  ## Restrictions are not parents; unlabelled classes are left out; IRIs are
  ## shortened to OBO-style ids.
  expect_setequal(owl$id, c("T:0000001", "T:0000002", "T:0000003", "T:0000004", "T:0000005"))
  expect_equal(sort(owl$parents[[which(owl$id == "T:0000003")]]), c("T:0000002", "T:0000005"))
  expect_true(owl$obsolete[owl$id == "T:0000004"])
  expect_equal(owl$definition[owl$id == "T:0000001"], "Anything at all.")
  obo <- attr(ontologyToTree(onto("tiny.obo")), "ontology")
  obo <- obo[obo$id %in% owl$id, ]
  owl <- owl[match(obo$id, owl$id), ]
  expect_equal(owl$name, obo$name)
  expect_equal(lapply(owl$parents, sort), lapply(obo$parents, sort))
})

test_that("search finds terms by name or id, at every place they appear", {
  o <- ontologyToTree(onto("tiny.obo"))
  ## "cell" is under two parents: two paths.
  paths <- o$search("cell")
  expect_true(list(c("entity", "material entity", "cell")) %in% list(paths[[1]]) ||
                any(vapply(paths, identical, logical(1), c("entity", "material entity", "cell"))))
  expect_true(any(vapply(paths, identical, logical(1), c("entity", "tissue part", "cell"))))
  ## The obsolete "old cell" is found under the obsolete node.
  expect_true(any(vapply(paths, identical, logical(1), c("obsolete", "old cell"))))
  ## By id, case-insensitively.
  expect_equal(o$search("t:0000005"), list(c("entity", "tissue part")))
  expect_length(o$search("no such term"), 0)
  ## Capped.
  expect_length(o$search("t:", limit = 3), 3)
})

test_that("search paths really exist in the tree", {
  o <- ontologyToTree(onto("tiny.obo"))
  for (p in o$search("e")) {
    parent <- list(); kids <- o$children(character(0), list())
    for (k in seq_along(p)) {
      expect_true(p[k] %in% names(kids), info = paste(p, collapse = "/"))
      if (k < length(p)) {
        node <- kids[[p[k]]]
        kids <- o$children(p[seq_len(k)], list(stid = attr(node, "stid")))
      }
    }
  }
})

test_that("bad input gives clear errors", {
  expect_error(ontologyToTree(onto("no-such-file.obo")), "existing .obo or .owl")
  txt <- tempfile(fileext = ".txt"); writeLines("hello", txt)
  expect_error(ontologyToTree(txt), "format")
  empty <- tempfile(fileext = ".obo"); writeLines("format-version: 1.2", empty)
  expect_error(ontologyToTree(empty), "No terms")
  expect_error(ontologyToTree(onto("tiny.obo"), format = "csv"))
})

test_that("an ontology works with renderTree(lazy = TRUE) end to end", {
  o <- ontologyToTree(onto("tiny.obo"))
  st <- new.env(parent = emptyenv())
  st$spec <- normaliseLazySpec(o); st$generation <- 1L; st$session <- NULL
  st$offered <- new.env(parent = emptyenv()); offerPath(st, character(0))
  req <- function(b) list(REQUEST_METHOD = "POST", rook.input = list(read = function(...) charToRaw(b)))
  top <- jsonlite::fromJSON(lazyRequestHandler(st, req('{"paths":[[]],"generation":1}'))$content, simplifyVector = FALSE)
  expect_equal(vapply(top$nodes[[1]]$children, `[[`, "", "text"), c("entity", "obsolete"))
  r <- jsonlite::fromJSON(lazyRequestHandler(st, req('{"search":"cell","generation":1}'))$content, simplifyVector = FALSE)
  expect_gte(length(r$matches), 3)
})

## --- Regressions from adversarial testing (#33) ----------------------------

obo_file <- function(lines) {
  f <- tempfile(fileext = ".obo")
  writeLines(lines, f, useBytes = TRUE)
  f
}
term <- function(...) c("[Term]", ..., "")
top_names <- function(o) names(o$children(character(0), list()))

test_that("search cannot hang on a cycle or a many-parent lattice", {
  ## A cycle with no way to a root.
  o <- ontologyToTree(obo_file(c(term("id: R", "name: root"), term("id: A", "name: a", "is_a: B", "is_a: C"),
                                 term("id: B", "name: b", "is_a: A"), term("id: C", "name: c", "is_a: A"),
                                 term("id: X", "name: target", "is_a: A"))))
  t <- system.time(r <- o$search("target", 5))[["elapsed"]]
  expect_lt(t, 2)
  ## The cycle is broken at the top, so the target is still found, by real
  ## paths that never repeat a term.
  expect_gte(length(r), 1)
  expect_true(all(vapply(r, function(p) !anyDuplicated(p), logical(1))))
  ## A diamond lattice 30 levels deep: 2^30 paths, but the limit stops it.
  lines <- term("id: L0a", "name: top")
  for (l in 1:30) for (x in c("a", "b")) {
    lines <- c(lines, term(sprintf("id: L%d%s", l, x), sprintf("name: n%d%s", l, x),
                           sprintf("is_a: L%da", l - 1), if (l > 1) sprintf("is_a: L%db", l - 1)))
  }
  lines <- c(lines, term("id: LEAF", "name: leafy", "is_a: L30a", "is_a: L30b"))
  o <- ontologyToTree(obo_file(lines))
  t <- system.time(r <- o$search("leafy", 10))[["elapsed"]]
  expect_lt(t, 2)
  expect_length(r, 10)
})

test_that("terms in a pure cycle, or their own parent, still appear in the tree", {
  o <- ontologyToTree(obo_file(c(term("id: R", "name: root"), term("id: A", "name: a", "is_a: B"),
                                 term("id: B", "name: b", "is_a: A"), term("id: S", "name: self", "is_a: S"))))
  expect_setequal(top_names(o), c("root", "a", "self"))
  top <- o$children(character(0), list())
  expect_equal(names(o$children("a", list(stid = attr(top$a, "stid")))), "b")
  expect_null(attr(top$self, "stchildren"))
})

test_that("a live term under an obsolete parent is found, or shown at the top", {
  f <- obo_file(c(term("id: R", "name: root"), term("id: O", "name: gone", "is_obsolete: true"),
                  term("id: L", "name: livekid", "is_a: O")))
  expect_equal(ontologyToTree(f)$search("livekid"), list(c("obsolete", "gone", "livekid")))
  o <- ontologyToTree(f, obsolete = NULL)
  expect_setequal(top_names(o), c("root", "livekid"))
  expect_equal(o$search("livekid"), list("livekid"))
})

test_that("OBO details: qualifiers, obsolete flags, ids, escapes, formatting", {
  terms <- attr(ontologyToTree(obo_file(c(
    term("id: R", "name: root"),
    term("id: K", "name: kid", 'is_a: R {comment="see!"} ! root'),
    term("id: Q", "name: q", 'is_obsolete: true {source="doi:10.1002/x"}'),
    term("id: T", "name: t", "is_obsolete: True ! comment"),
    term("id: C ! a comment", "name: c", "is_a: R"),
    term("id: D", "name: d", 'def: "line one\\nline two\\ttab" []', "is_a: R"),
    term("id: U", "name: u", 'def: "unterminated [x]', "is_a: R"),
    c("  [term] ! a note", "  id: I", "  name : indented", "is_a: R", "")
  ))), "ontology")
  get <- function(id, col) terms[[col]][terms$id == id]
  expect_equal(get("K", "parents")[[1]], "R")
  expect_true(get("Q", "obsolete"))
  expect_true(get("T", "obsolete"))
  expect_true("C" %in% terms$id)
  expect_equal(get("D", "definition"), "line one\nline two\ttab")
  expect_equal(get("U", "definition"), "unterminated")
  expect_equal(get("I", "name"), "indented")
  expect_equal(get("I", "parents")[[1]], "R")
})

test_that("nameless placeholder stanzas are left out; a nameless term with a parent is kept", {
  terms <- attr(ontologyToTree(obo_file(c(term("id: R", "name: root"), term("id: P", "alt_id: X:1"),
                                          term("id: N", "is_a: R")))), "ontology")
  expect_false("P" %in% terms$id)
  expect_equal(terms$name[terms$id == "N"], "N")
})

test_that("a term given twice keeps the parents of both copies", {
  f <- obo_file(c(term("id: R", "name: root"), term("id: S", "name: second root"),
                  term("id: A", "name: a", "is_a: R"), term("id: A", "name: a again", "is_a: S")))
  expect_warning(o <- ontologyToTree(f), "more than once")
  terms <- attr(o, "ontology")
  expect_equal(sort(terms$parents[[which(terms$id == "A")]]), c("R", "S"))
  expect_equal(terms$name[terms$id == "A"], "a")
})

test_that("a file that is not valid UTF-8 is read, with a warning", {
  f <- obo_file(c("[Term]", "id: A", paste0("name: caf", rawToChar(as.raw(0xe9)))))
  expect_warning(o <- ontologyToTree(f), "not valid UTF-8")
  expect_equal(attr(o, "ontology")$name, "caf?")
})

test_that("the obsolete group does not replace a real term of the same name", {
  o <- ontologyToTree(obo_file(c(term("id: X:1", "name: obsolete"), term("id: X:2", "name: child", "is_a: X:1"),
                                 term("id: X:3", "name: old", "is_obsolete: true"))))
  top <- o$children(character(0), list())
  expect_equal(names(top), c("obsolete", "obsolete"))
  expect_equal(attr(top[[1]], "stid"), "X:1")
  expect_equal(names(o$children("obsolete", list(stid = "X:1"))), "child")
})

test_that("the obsolete argument is checked", {
  f <- test_path("ontology", "tiny.obo")
  for (bad in list(TRUE, NA, c("x", "y"), "", 1)) {
    expect_error(ontologyToTree(f, obsolete = bad), "`obsolete` must be", info = deparse(bad))
  }
  expect_silent(ontologyToTree(f, format = "OBO"))
})

test_that("search ignores empty or missing text", {
  o <- ontologyToTree(test_path("ontology", "tiny.obo"))
  expect_identical(o$search(NA), list())
  expect_identical(o$search(character(0)), list())
  expect_identical(o$search(""), list())
})

test_that("OWL: rdf:ID, typed rdf:Description, nested parents, languages, deprecated forms", {
  skip_if_not_installed("xml2")
  f <- tempfile(fileext = ".owl")
  writeLines('<?xml version="1.0"?>
<rdf:RDF xmlns:owl="http://www.w3.org/2002/07/owl#" xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
         xmlns:rdfs="http://www.w3.org/2000/01/rdf-schema#" xmlns:obo="http://purl.obolibrary.org/obo/"
         xml:base="http://example.org/onto">
  <owl:Class rdf:ID="Animal"><rdfs:label xml:lang="fr">animal (fr)</rdfs:label><rdfs:label xml:lang="en">animal</rdfs:label></owl:Class>
  <rdf:Description rdf:about="http://example.org/onto#Cat">
    <rdf:type rdf:resource="http://www.w3.org/2002/07/owl#Class"/>
    <rdfs:label xml:lang="fr">chat</rdfs:label><rdfs:label xml:lang="en">cat</rdfs:label>
    <rdfs:subClassOf><owl:Class rdf:about="http://example.org/onto#Animal"/></rdfs:subClassOf>
    <obo:IAO_0000115>A small feline.</obo:IAO_0000115>
  </rdf:Description>
  <owl:Class rdf:about="http://example.org/onto#Old"><rdfs:label>old</rdfs:label><owl:deprecated> 1 </owl:deprecated></owl:Class>
  <owl:Class rdf:about="http://a.org/onto#Cell"><rdfs:label>cell a</rdfs:label></owl:Class>
  <owl:Class rdf:about="http://b.org/other/Cell"><rdfs:label>cell b</rdfs:label></owl:Class>
</rdf:RDF>', f)
  terms <- attr(ontologyToTree(f), "ontology")
  expect_equal(terms$name[terms$id == "http://example.org/onto#Animal"], "animal")
  cat_row <- terms$id == "http://example.org/onto#Cat"
  expect_equal(terms$name[cat_row], "cat")
  expect_equal(terms$definition[cat_row], "A small feline.")
  expect_equal(terms$parents[[which(cat_row)]], "http://example.org/onto#Animal")
  expect_true(terms$obsolete[terms$id == "http://example.org/onto#Old"])
  ## IRIs outside the OBO PURLs are kept whole, so these do not collide.
  expect_true(all(c("http://a.org/onto#Cell", "http://b.org/other/Cell") %in% terms$id))
})

test_that("an OWL file that is not RDF/XML gives a clear error", {
  skip_if_not_installed("xml2")
  f <- tempfile(fileext = ".owl"); writeLines("@prefix owl: <http://www.w3.org/2002/07/owl#> .", f)
  expect_error(ontologyToTree(f), "as OWL in RDF/XML")
})

test_that("pathsTo itself never follows a cycle round", {
  ## breakCycles() normally removes cycles before search, so pathsTo() is
  ## tested on its own with a cycle that does reach a root: A <-> B, B is_a R.
  ids <- c("R", "A", "B", "X")
  terms <- data.frame(id = ids, name = tolower(ids), definition = NA_character_,
                      obsolete = FALSE, stringsAsFactors = FALSE)
  terms$parents <- I(list(character(0), c("B"), c("A", "R"), "A"))
  row <- stats::setNames(seq_along(ids), ids)
  budget <- new.env(); budget$n <- 1e5L
  t <- system.time(p <- pathsTo("X", stats::setNames(terms$parents, ids), terms, row, "obsolete", 1000L, budget))
  expect_lt(t[["elapsed"]], 2)
  expect_equal(p, list(c("r", "b", "a", "x")))
  ## The budget stops a search with too many paths.
  budget$n <- 2L
  expect_error(pathsTo("X", stats::setNames(terms$parents, ids), terms, row, "obsolete", 1000L, budget), "too many paths")
})
