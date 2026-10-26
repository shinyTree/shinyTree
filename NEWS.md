# shinyTree 0.3.1.9000

## Fixes from a review of the whole package

- `input$tree` now holds the tree as soon as it loads, and is updated when the
  server sends a new tree with `renderTree()` or `updateTree()`: it used to stay
  `NULL` until the user clicked, and kept a replaced tree -- and its selection of
  nodes that no longer exist -- until then. A tree is reported once, and only
  if it differs from what `input$tree` holds, so an observer of `input$tree`
  that sends the tree back still does not loop (#36).
- A tree re-rendered after an error in its render function -- a `req()` or
  `validate()` that fails and then passes -- no longer stops updating
  `input$tree`. jsTree's teardown removed the input binding's event handlers.
- Trees whose id contains `.` (or another character with a meaning in CSS
  selectors) now work. Their options, checkboxes and search were ignored and
  `input$tree` was never set, and with `types` the page threw an error that
  stopped every tree on it from reporting.
- `updateTree()`, `expandTree()` and `collapseTree()` on a tree that has not
  been shown yet -- in a tab not yet opened -- are applied once it is, rather
  than throwing an error in the browser.
- Deleting the last node with the context menu now clears `input$tree`; it
  kept the deleted nodes.
- A node attribute such as `sttype_label` or `sticonColor` is no longer taken
  for `sttype` or `sticon`: attributes were matched by prefix. The same applied
  to the `text`, `name`, `icon` and `state` fields in `treeToJSON()`.
- Numbers in node data are sent at full precision; they were rounded to four
  digits, so a numeric id such as `123456.789` came back changed.
- `sttype`, `sticon` and `sttooltip` given as a number, a factor (from a data
  frame column, say), `NA` or more than one value no longer stop the tree from
  rendering.
- `sticon` values in current Font Awesome form -- `"fa-solid fa-folder"`,
  `"fas fa-file"`, `"fa-file fa-lg"` -- other icon sets, and image file names
  without a path are now used as given. Only a bare name such as `"file"` is
  expanded, to `"fa fa-file"`.
- R's own attributes on a node -- a data frame's `class` and `row.names`, say --
  are no longer sent as node data.
- A node without a name is now an error naming the problem, rather than a node
  labelled `{}`.
- JSON from `jsonlite::toJSON()` can be passed to `renderTree()` and
  `updateTree()`; it reached the browser already parsed and failed there.
- `renderTreeAsync()` is now built on `renderTree()`: it repopulates a tree in
  a modal that is reopened (#79), works with `bindCache()`, and an error in a
  rejected promise is shown in place of the tree.
- `options(shinyTree.defaultParser = "tree")` set before the package is loaded,
  e.g. in `.Rprofile`, is no longer overwritten, and the package can be loaded
  again in the same session, as `devtools::load_all()` does. The `"tree"`
  parser warns about siblings with the same name, which a data.tree cannot keep
  apart.
- `treeToJSON()` keeps the `checked` state, and gives an empty tree rather than
  one blank node for a root without children.
- `dfToTree()` builds a tree whose branches end at different depths, as a
  folder listing gives, where it stopped with an error; it gives an empty list
  for a data frame with no rows, and names any `hierarchy` column the data
  frame lacks. `treeToDf()` handles an empty tree and empty branches.
- `shinyTree()` accepts `search = NULL` and `animation = NULL`, writes ids into
  its scripts safely, and puts the search script after the search box rather
  than inside it. Search also reacts to pasting, not only typing.
- Examples `16-async`, `20-api` and `23-file-icons` work again, `21-options` has
  been rewritten to show what the options do since #36 and can be started with
  `runApp()`, and `14-files` lists every file.
- Documentation: every exported function documents what it returns; new
  sections and examples for `renderTree()`, `updateTree()`,
  `renderTreeAsync()`, `get_checked()`, `dfToTree()` and `treeToDf()`;
  `format = "classid"`, the `shinyTree.defaultParser` option, the contextmenu
  turning checkboxes off and the `sticon` forms are documented; and the
  `ontologyToTree()` example runs on the bundled ontology.
- Requires R >= 3.3.0 and shiny >= 1.6.0, which the package already needed.
  stringr, htmlwidgets and methods are no longer used, and the unused copies of
  jQuery and require.js are no longer bundled.
- Tests: an example app that fails to start, or a browser test app that errors,
  now fails the tests instead of skipping them; every example is started and
  checked; and fixed waits are replaced by waiting for what the test expects.

## New features and other fixes

- New `ontologyToTree()` reads an ontology from an OBO (`.obo`) or OWL
  (`.owl`, RDF/XML) file and returns what `renderTree(lazy = TRUE)` needs, so
  even a large ontology such as the Gene Ontology's 48,000 terms is shown a
  level at a time. The hierarchy is `is_a`; a term with several parents
  appears under each; obsolete terms are greyed out under an `obsolete` node;
  each node shows its id and definition on hover; search finds terms by name
  or id anywhere in the ontology. New example `28-ontology` browses the
  ontology from the issue, or an uploaded file (#33).
- A lazy tree that is rebuilt now reports the new tree to the server, so
  `input$tree` no longer keeps the old tree's selection.
- New `renderTree(lazy = TRUE)` loads a tree's nodes from the server as they
  are opened, for trees too large to send whole. The render expression returns
  `function(path)` (or `function(path, node)`, to tell same-named siblings
  apart by their `st*` attributes) giving a node's children; children with
  `stchildren = TRUE` are fetched when opened. With `shinyTree(search = TRUE)`
  the whole tree is searched on the server, by walking it or with a `search`
  function, and the branches leading to the matches are fetched and opened.
  `state = TRUE` reopens remembered branches after a reload. Branches needed
  at once are fetched in a single request. Requests are checked: only branches
  the server has sent can be asked for, with the server's own node data, and
  errors follow `shiny.sanitize.errors`. `updateTree()` and `expandTree()` stop
  with an error on a lazy tree. See the new `27-lazy` example (#11).
- Unticking a child of a ticked parent with `tie_selection = FALSE` and
  `state = TRUE` is now remembered: the state was saved before the parent was
  unticked, so a reload ticked everything again.
- `renderTree()` given a function now stops with a pointer to `lazy = TRUE`,
  rather than leaving the tree empty with a JSON error in the browser.
- A tree now repopulates when its element is removed from the page and added
  back -- inside a `modalDialog` that is dismissed and reopened, for example.
  Contents are pushed to the client rather than returned as an output value, so
  Shiny had nothing to replay and would not re-run the render function. The
  client now reports binding an empty tree and the contents are resent (#79).
- Fixed `types` being ignored for trees nested more than one Shiny module deep.
  Only the first hyphen of the output id was replaced when looking up the types
  spec on the client, so the lookup missed and the tree failed to render (#129).
- Search now hides non-matching nodes with the `proton` theme. jsTree marked
  them correctly but proton's stylesheet omits the rule that hides them, which
  the other themes ship, so every node stayed on screen. shinyTree now
  declares it in its own stylesheet (#113).
- New `stcssclass` node attribute gives a node one or more CSS classes, so an
  app's own CSS can style it. `stclass`, despite its name, has only ever been
  data and is unchanged (#58).
- Node data that R refuses as an attribute -- a `class` field holding a list,
  as trees built from XML often have -- no longer stops `input$tree` from being
  read. It comes back under an `st` prefix, e.g. `stclass` (#58).
- New `shinyTree(state = TRUE)` remembers which nodes were open, selected and
  checked in the browser, and restores them when the page is reloaded. Each
  tree is stored under its own key (`"shinyTree-<id>"` by default, or pass a
  string). Nodes are matched by their path from the root, and only the first
  tree after a reload is restored. When the remembered state disagrees with the
  `stopened`/`stselected`/`stchecked` attributes the tree was rendered with,
  the remembered state wins; `state_priority = "code"` keeps the app's instead
  (#19).
- A tree is no longer sent to the browser twice on page load when its render
  takes more than about 100ms. The check that repopulates a tree whose element
  is added back (#79) also fired for brand-new trees, so the server resent the
  tree and jsTree rebuilt it. It now only fires for a tree that was already sent.
- New `sttooltip` node attribute: text shown when the pointer rests on a node's
  label. `?renderTree` now documents all of the `st*` node attributes (#66).
- Fixed two broken stylesheet links to the Font Awesome CDN on every page with
  a tree, which each failed with a 403 in the browser console. Font Awesome
  itself was unaffected and still loads with its integrity hash.
- A tree now works in R Markdown documents with `runtime: shiny`. Its scripts
  and stylesheets were written into the page's `<head>`, which R Markdown
  drops, so jsTree never loaded and the tree stayed empty. They are now
  declared as HTML dependencies, which also means each is included once per
  page however many trees it has (#18).
- New `expandTree()` and `collapseTree()` open or close every node of a tree
  from the server, for "Expand all" / "Collapse all" buttons. The tree is not
  resent, so selection, checkboxes and search are kept, and `input$tree`
  reports the new open state (#42).
- `get_selected()` and `get_checked()` gain `leaves_only`. With
  `leaves_only = TRUE` they return only nodes that have no children, however
  deep each branch goes, so selected parents -- and, with `three_state`, the
  parents jsTree checks automatically -- are left out (#98).
- Large trees are much faster to render and to read back. Building a tree's
  JSON grew the node list one element at a time and serialised it node by
  node, and reading `input$tree` with `get_selected()`/`get_checked()` did the
  same with the matches, so both were quadratic in the size of the tree. For a
  30,000-node tree, rendering went from 14s to 2s and `get_checked()` from 15s
  to under 1s. The JSON sent to the browser and the values returned are
  unchanged (#83).
- `renderTree()` can now be wrapped in `shiny::bindCache()`. It was refused
  outright, and since a tree is sent as a message rather than returned as the
  output value, caching that value would have left the tree empty on every cache
  hit. The tree's JSON is cached instead and sent again on a hit. A tree that is
  removed from the page and added back (#79) now gets the tree last sent rather
  than a fresh evaluation of the render expression, matching how Shiny replays
  other outputs (#109).
- Shiny no longer warns about a shared input/output id for every tree. A
  shinyTree is an input and an output under the same id by design, so the
  browser console warning -- and, with `shiny::devmode()`, the on-page error
  panel -- was a false alarm in every app. A shared id that involves anything
  other than a tree is still reported (#110).
- The `04-selected` example now shows `get_selected()` and `get_checked()`
  side by side, with toggles for `checkbox`, `tie_selection`, `whole_node` and
  `three_state` and an explanation of how each combination behaves.
  `24-checkbox-options` shows both too, and `?shinyTree` documents the
  difference between checked and selected nodes (#136).
- With `tie_selection = FALSE`, `input$tree` now reflects `three_state`
  cascades. It was read before jsTree applied the cascade, so checking a parent
  reported only the parent rather than its children too, and unchecking a child
  left its parent reported as checked.
- Fixed `get_checked()` returning nothing when `checkbox = TRUE` and
  `tie_selection = FALSE` (#121).
- Fixed `get_checked()` returning nothing for ticked boxes under the default
  `tie_selection = TRUE`. jsTree treats checked and selected as the same thing
  in that mode, so a selected node is now reported as checked, matching
  jsTree's own `is_checked()`/`get_checked()` (#121).
- `updateTree()` no longer reports its own refresh back to the server. Calling
  it from an observer watching `input$tree` could otherwise loop indefinitely
  when `options(shinyTree.refresh)` or `options(shinyTree.setState)` was
  enabled. Refreshes from other sources are still reported, and the two
  options are now documented (#36).
- Node attribute values are no longer trimmed on their way back to R, so
  padded keys and fixed-width codes stored in `st*` attributes survive a round
  trip. Node *names* are still trimmed, which is deliberate (#3).
- Fixed `get_selected()`/`get_checked()` returning nothing for trees rendered
  from `treeToJSON()`. A node state without a `checked` key made the input
  handler fail with "argument is of length zero", so `input$tree` was never
  populated (#103).
- Fixed duplicate-named sibling nodes collapsing into a single entry, so
  `get_selected()`/`get_checked()` now report the correct node and its own
  attributes rather than only the last sibling (#104).

# shinyTree 0.3.1

- Checkbox functionality.
- Bug fixes.

# shinyTree 0.3.0

- New function `set_node_attrs()` to set distinct node or leaf attributes
  (useful for creating file tree icons).
- New functions `dfToTree()` and `treeToDf()` convert between a data frame and
  a tree.

# shinyTree 0.2.7

- Added context menu support.
- Added support for promises/future to render a tree asynchronously.
- Added additional examples for file browser and metadata retrieval.
- Added support to display various icons using types and specifying tree items
- Added support for conversion from/to data.tree

# shinyTree 0.2.6

- Many improvements to expose more jsTree options.
- Tree can now be correctly updated (see new examples).

# shinyTree 0.2.0

- BREAKING CHANGE: Model the `input$list`-style lists after the list structure
  provided to `renderTree`, meaning that they are now a simple list with the
  same `st`- prefixed attributes. See `inst/examples/05-structure`.
- BREAKING CHANGE: removed `selected` attribute from `shinyTree()` in favor of
  `get_selected()`. See `inst/examples/04-selected/`.

# shinyTree 0.1.0

- Initial Release
