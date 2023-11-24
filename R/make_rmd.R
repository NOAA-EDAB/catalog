#' Create markdown file using parsed github issue
#' 
#' Template rmd hardcoded in function. Interspersed with data from github issue
#' 
#' @param listobject list. parsed issue from github
#' 
#' @return creates an rmd in chapters folder

make_rmd <- function(listobject){
  
  # line for the indicator name
  #indicator_name <- listobject$name
  # create filename based on indicator name. Replace all spaces with underscore
  #filename <- gsub("\\s+","_",indicator_name)
  filename <- listobject$ecodataname
  
  # create rmd with name of indicator
  con <- file(here::here("chapters",paste0(filename,".rmd")),open="w")
     
  # start to create the Rmd
  #cat(paste0("# ",stringr::str_to_title(indicator_name)),append=T,fill=T,file=con)    
  cat(paste0("# ",listobject$indicatorname),append=T,fill=T,file=con)    
  cat("",append=T,fill=T,file=con) # add space
  cat(paste0("**Description**: ",listobject$description),append=T,fill=T,file=con)
  cat("",append=T,fill=T,file=con) # add space
  cat(paste0("**Indicator family**: ",listobject$family),append=T,fill=T,file=con)
  cat("",append=T,fill=T,file=con) # add space
  cat(paste0("**Contributor(s)**: ",listobject$contributors),append=T,fill=T,file=con)
  cat("",append=T,fill=T,file=con) # add space
  cat(paste0("**Affiliations**: ",listobject$affiliations),append=T,fill=T,file=con)
  cat("",append=T,fill=T,file=con) # add space
  
  # chunk script
  cat("```{r echo=FALSE}",append=T,fill=T,file=con)
  cat("knitr::opts_chunk$set(echo = F)",append=T,fill=T,file=con)
  cat("library(ecodata)",append=T,fill=T,file=con)
  cat("```",append=T,fill=T,file=con)
  
  cat("## What is this?",append=T,fill=T,file=con)
  cat(listobject$whatsthis,append=T,fill=T,file=con)
  cat("",append=T,fill=T,file=con) # add space
  
  cat("### Visualizations",append=T,fill=T,file=con)
  cat(listobject$visualizations,append=T,fill=T,file=con)
  cat("",append=T,fill=T,file=con) # add space
  
  ### section to run plot functions from ecodata
  
  
  
  cat("### Indicator statistics ",append=T,fill=T,file=con)
  cat(paste0("Spatial scale: ",listobject$indicatorStatsSpatial),append=T,fill=T,file=con)
  cat("",append=T,fill=T,file=con) # add space
  cat(paste0("Temporal scale: ",listobject$indicatorStatsTemporal),append=T,fill=T,file=con)
  cat("",append=T,fill=T,file=con) # add space
  
  # r chunk for autostats
  cat(paste0("```{r autostats_",filename,"}"),append=T,fill=T,file=con)
  cat("# Either from Contributor or ecodata",append=T,fill=T,file=con)
  cat("```",append=T,fill=T,file=con)
  cat("",append=T,fill=T,file=con) # add space
  
  
  cat("### Implications",append=T,fill=T,file=con)
  cat(listobject$implications,append=T,fill=T,file=con)
  cat("",append=T,fill=T,file=con) # add space
  
  
  cat("## Get the data",append=T,fill=T,file=con)
  cat("",append=T,fill=T,file=con) # add space
  
  cat(paste0("**Point of contact**: [",listobject$poc,"](",listobject$poc,"){.email}"),append=T,fill=T,file=con)
  cat("",append=T,fill=T,file=con) # add space
  
  cat(paste0("**ecodata name**: `ecodata::",listobject$ecodataname,"`"),append=T,fill=T,file=con)
  cat("",append=T,fill=T,file=con) # add space
  
  cat("**variable names**",append=T,fill=T,file=con)
  cat("",append=T,fill=T,file=con) # add space
  
  # r code chunk to make table
  cat(paste0("```{r vars_",listobject$ecodataname,"}"),append=T,fill=T,file=con)
  cat("# Pull all var names",append=T,fill=T,file=con)
  cat(paste0("vars <- ecodata::",listobject$ecodataname," |>"),append=T,fill=T,file=con)
  cat("   dplyr::select(Var, Units) |>",append=T,fill=T,file=con)
  cat("   dplyr::distinct()",append=T,fill=T,file=con)
  cat("",append=T,fill=T,file=con) # add space
  cat("DT::datatable(vars)",append=T,fill=T,file=con) # add space
  cat("```",append=T,fill=T,file=con)
  
  
  
  cat("**tech-doc link**",append=T,fill=T,file=con)
  
  cat(paste0("<https://noaa-edab.github.io/tech-doc/",listobject$ecodataname,".html>"),append=T,fill=T,file=con)
  cat("",append=T,fill=T,file=con) # add space
  
  
  cat("## References",append=T,fill=T,file=con)
  cat("",append=T,fill=T,file=con) # add space
  
  
  #close the connection
  close(con)
  
 
}