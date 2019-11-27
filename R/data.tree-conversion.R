#' Converts a data.tree to a JSON format
#' 
#' Walk through a \code{\link[data.tree]{data.tree}} and constructs a JSON string,
#' which can be rendered by shinyTree. 
#' 
#' The JSON string generated follows the 
#' \href{https://www.jstree.com/docs/json}{jsTree specifications}. In particular 
#' it encodes children nodes via the \sQuote{children} slot. 
#' 
#' All atomic or list slots of a node in the tree are stored in a data slot in 
#' the resulting JSON.
#' 
#' If the user wants to store some slots not in the data slot but on the top 
#' level of the node, parameter \code{topLevelSlots} can be used. This is useful
#' for additional parameters such as \sQuote{icon}, \sQuote{li_attr} or 
#' \sQuote{a_attr}, which jsTree expect to be on the top level of the node.
#' 
#' An example of how to make use of this functionality can be found in the
#' example folder of this library.
#'
#' @param tree the data.tree which should be parses
#' @param keepRoot logical. If \code{FALSE} (default) the root node from the tree
#'        is pruned
#' @param topLevelSlots determines which slots should be moved to the top level of the 
#'        node. If \code{default} or \code{NULL} slots 
#'        \href{https://www.jstree.com/docs/json}{used in the jsTree JSON} 
#'        are kept on the top level, while any other atomic / list slots from the tree
#'        are stored in an own slot called \sQuote{data}. If \code{all} *all* nodes are
#'        stored on the top level. Alternatively, it can be an explicit vector of
#'        slot names which should be kept. In the latter case it is the user's
#'        responsibility to ensure that jsTree slots stay on the top level.
#' @param createNewId logical. If \code{TRUE} a new id will be generated. Any old \sQuote{id} 
#'        will be stored in \sQuote{id.orig} and a warning will be issued, If \code{FALSE},
#'        any existing id will be re-used.
#' @param pretty logical. If \code{TRUE} the resulting JSON is prettified
#' @return a JSON string representing the data.tree
#' @section Note:
#' \code{\link{updateTree}} and \code{\link{renderTree}} need an unevaluated JSON 
#' string. Hence, this function returns a string rather than the JSON object itself.
#' @author Thorn Thaler, \email{thorn.thaler@@thothal.at} 
#' @export
treeToJSON <- function(tree, 
                       keepRoot = FALSE,
                       topLevelSlots = c("default", "all"),
                       createNewId = TRUE,
                       pretty = FALSE) {
  ## match against "default"/"all", if this returns an error we take topLevelSlots as is
  ## i.e. a vector of names to keep
  if (!requireNamespace("data.tree", quietly = TRUE)) {
    msg <- paste("library", sQuote("data.tree"), "cannot be loaded. Try to run",
                 sQuote("install.packages(\"data.tree\")"))
    stop(msg, domain = NA)
  }
  nodesToKeep <- list(default = c("id", "text", "icon", "state",
                                  "li_attr", "a_attr", "type"),
                      all     = NULL)
  topLevelSlots <- tryCatch(nodesToKeep[[match.arg(topLevelSlots)]],
                            error = function(e) topLevelSlots)
  node_to_list <- function(node, 
                           node_name = NULL) {
    fields <- mget(node$fields, node)
    NOK <- sapply(fields, function(slot) !is.atomic(slot) && !is.list(slot))
    if (any(NOK)) {
      msg <- sprintf(ngettext(length(which(NOK)),
                              "unsupported slot of type %s at position %s",
                              "unsupported slots of types %s at positions %s"),
                     paste0(dQuote(sapply(fields[NOK], typeof)),
                            collapse = ", "),
                     paste0(sQuote(names(fields)[NOK]),
                            collapse = ", "))
      warning(msg,
              domain = NA)
      fields[NOK] <- NULL
    }
    if (is.null(fields$text)) {
      fields$text <- if(!is.null(fields$name)) fields$name else node_name
    }
    fields$icon <- fixIconName(fields$icon)
    if (!is.null(fields$state)) {
      valid_states <- c("opened", "disabled", "selected", "loaded")
      states_template <- stats::setNames(rep(list(FALSE), length(valid_states)),
                                         valid_states)
      NOK <- !names(fields$state) %in% valid_states
      if (any(NOK)) {
        msg <- sprintf(ngettext(length(which(NOK)),
                                "invalid state %s",
                                "invalid states %s"),
                       paste0(dQuote(names(fields$state)[NOK]),
                              collapse = ", "))
        warning(msg,
                domain = NA)
      }
      states_template[names(fields$state[!NOK])] <- fields$state[!NOK]
      fields$state <- states_template
    }
    if (is.null(topLevelSlots)) {
      slots_to_move <- character(0)
    } else {
      slots_to_move <- names(fields)[!names(fields) %in% topLevelSlots]
    }
    data_slot <- fields[slots_to_move]
    if (length(data_slot)) {
      fields$data <- data_slot
      fields[slots_to_move] <- NULL
    }
    if (!is.null(node$children)) {
      ## purrr::imap would make code cleaner but did not want to add another dependency
      ## unname needed to create an JSON array as opposed to an JSON object
      fields$children <- unname(lapply(names(node$children), 
                                       function(i) node_to_list(node$children[[i]],
                                                                i)))
    }
    fields
  }
  ## clone tree as we do not want to alter the original tree
  tree <- data.tree::Clone(tree)
  nodes <- data.tree::Traverse(tree, filterFun = data.tree::isNotRoot)
  old_ids <- data.tree::Get(nodes, "id")
  if (createNewId) {
    if (any(!is.na(old_ids))) {
      warning(paste("slot",
                    dQuote("id"), 
                    "will be stored in",
                    dQuote("id.orig")),
              domain = NA)
      data.tree::Set(nodes, id.orig = old_ids)
    }
    new_ids <- seq_along(nodes)
  } else {
    if (any(is.na(old_ids)) ||
        any(duplicated(old_ids))) {
      warning(paste("old ids are invalid (duplicated values or NA),",
                    "creating new ids"),
              domain = NA)
      new_ids <- seq_along(nodes)
    } else {
      new_ids <- old_ids
    }
  }

  data.tree::Set(nodes, id = new_ids)
  treeList <- node_to_list(tree)
  if (!keepRoot) {
    ## to prune off the root node return the first children list
    treeList <- treeList$children
  }
  ## use as.character b/c updateTree needs an unparsed JSON string, as 
  ## the parsing is done in shinyTree.js
  as.character(jsonlite::toJSON(treeList, 
                                auto_unbox = TRUE, 
                                pretty = pretty))
}