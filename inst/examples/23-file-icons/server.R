library(shiny)
library(shinyTree)

#' @author Sandro Raabe \email{sa.ra.online@posteo.de} 
shinyServer(function(input, output, session) {
  for(pkg in c("tidyr", "stringr", "tibble"))
    if(! pkg %in% installed.packages())
      stop(paste(pkg, " must be installed for this example to work"))
  
  output$tree <- renderTree({
    files <- data.frame(allFiles) %>%
      tidyr::separate(allFiles, 
                      as.character(seq_len(max(stringr::str_count(.$allFiles, "/")) + 1)), 
                      sep = "/",
                      fill = "right") %>% 
      tibble::as_tibble()
    
    set_node_attrs(dfToTree(files),
                   attr_name = "sttype", 
                   inner_val = "directory", 
                   leaf_val = "file")
    
  })
})