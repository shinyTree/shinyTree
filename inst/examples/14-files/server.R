library(shiny)
library(shinyTree)

#' @author Michael Bell \email{bellma@@lilly.com}
shinyServer(function(input, output, session) {
  
  
  listFiles <- function(maxDepth,path,currentDepth=1){
    dirs <- list.dirs(path,recursive = FALSE, full.names = FALSE)
    allFiles <- list.files(path,list.dirs(recursive = FALSE, full.names = FALSE))
    files <- setdiff(allFiles, dirs)
    if(length(dirs) != 0 && (maxDepth == 0 || currentDepth < maxDepth)){
      subtree <- append(lapply(dirs,
        function(nextDir){
          nextDir = structure(listFiles(maxDepth,file.path(path,nextDir),currentDepth+1),sttype="directory")
        }),files
      )
      names(subtree) <- append(dirs,files)
      subtree
    }else{
      subtree <- append(lapply(dirs,
        function(nextDir){
          structure(nextDir,sttype="directory")
        }),files
      )
      names(subtree) <- append(dirs,files)
      subtree
    }
  }
 
  treeStructure <- listFiles(3,"..")
  output$tree <- renderTree(treeStructure)
})