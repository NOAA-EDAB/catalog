#' Create markdown file using parsed github issue
#' 
#' Template rmd hardcoded in function. Interspersed with data from github issue
#' 
#' @param listobject parsed issue from github
#' 
#' 

# this list created after parsing issue
listobject <- list()
listobject$name <- "more forage fish"
listobject$description <- "a really long description"
listobject$family <- "the family of indicators"


make_rmd <- function(listobject){
  
  # line for the indicator name
  indicator_name <- listobject$name
  # create filenmae based on indicator name. Replace all spaces with underscore
  filename <- gsub("\\s+","_",indicator_name)
  
  # create rmd with name of indicator
  con <- file(here::here("chapters",paste0(filename,".rmd")),open="w")
     
  # start to create the Rmd
  cat(paste0("# ",stringr::str_to_title(indicator_name)),append=T,fill=T,file=con)    
  cat("",append=T,fill=T,file=con) # add space
  cat(paste0("**Description**: ",listobject$description),append=T,fill=T,file=con)
  cat("",append=T,fill=T,file=con) # add space
  cat(paste0("**Indicator family**: ",listobject$family),append=T,fill=T,file=con)

  #etc
  
  #close the connection
  close(con)
  
 
}
