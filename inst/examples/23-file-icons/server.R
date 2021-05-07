library(shiny)
library(shinyTree)

#' @author Sandro Raabe \email{sa.ra.online@posteo.de} 
shinyServer(function(input, output, session) {
    if(! "tidyr" %in% installed.packages())
      stop("tidyr must be installed for this example to work")
  
  output$tree <- renderTree({
    allFiles <- data.frame(path = list.files("../..", recursive = TRUE), stringsAsFactors = FALSE)
    max_depth <- max(vapply(strsplit(allFiles$path, "/"), length, FUN.VALUE = integer(1)))
    
    files <- tidyr::separate(allFiles,
                             path, 
                             as.character(seq_len(max_depth)), 
                             sep = "/",
                             fill = "right")
    
    set_node_attrs(dfToTree(files),
                   attr_name = "sttype", 
                   inner_val = "directory", 
                   leaf_val = "file")
    
  })
})
