#' Converts a data.tree to a JSON format
#' 
#' Walk through a \code{\link{data.tree}} and constructs a JSON string,
#' which can be rendered by shinyTree. The JSON string generated follows the 
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
#' @param tree, the data.tree which should be parses
#' @param topLevelSlots, a vector of slot names which should not be stored in
#'        the resulting data slot but on the top level of the node 
#' @param pretty, logical. If \code{TRUE} the resulting JSON is prettified
#'
#' @return a JSON string representing the data.tree
#' @section Note:
#' \code{\link{updateTree}} and \code{\link{renderTree}} need an unevaluated JSON 
#' string. Hence, this function returns a string rather than the JSON object itself.
#' @author Thorn Thaler, \email{thorn.thaler@@thothal.at} 
#' @export
treeToJSON <- function(tree, 
                       topLevelSlots = c("id", "text", "icon", "state",
                                         "li_attr", "a_attr"),
                       pretty = FALSE) {
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
      valid_states <- c("opened", "disabled", "selected")
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
      fields$state <- fields$state[!NOK]
    }
    slots_to_move <- names(fields)[!names(fields) %in% topLevelSlots]
    data_slot <- fields[slots_to_move]
    if (length(data_slot)) {
      fields$data <- data_slot
      fields[slots_to_move] <- NULL
    }
    if (!is.null(node$children)) {
      fields$children <- unname(lapply(names(node$children), function(i) node_to_list(node$children[[i]],
                                                                                      i)))
    }
    fields
  }
  ## clone tree as we do not want to alter the original tree
  tree <- Clone(tree)
  nodes <- Traverse(tree, filterFun = isNotRoot)
  old_ids <- Get(nodes, "id")
  if (any(!is.na(old_ids))) {
    warning(glue("slot {dQuote('id')} will be stored in {dQuote('id.orig')}"),
            domain = NA)
    Set(nodes, id.orig = old_ids)
  }
  Set(nodes, id = seq_along(nodes))
  ## use as.character b/c updateTree needs an unparsed JSON string, as 
  ## the parsing is done in shinyTree.js
  as.character(jsonlite::toJSON(node_to_list(tree)$children, 
                                auto_unbox = TRUE, 
                                pretty = pretty))
}