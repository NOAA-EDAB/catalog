#' Access GitHub API 
#' 
#' process issues found on the repo
#' 
#' 

# jsonlite::fromJSON('https://api.github.com/users/andybeet/repos')
#
repo <- 'https://api.github.com/repos/NOAA-EDAB/catalog/issues'
issues <- jsonlite::fromJSON(repo)
indices <- which(unlist(lapply(issues$labels,function(x) {if(length(x$name)==0){F}else{x$name=="submission"}})))
submissions <- issues$number[indices]

for (issuenum in submissions) {
  issue <- jsonlite::fromJSON(paste0(repo,"/",issuenum))
  body <- issue$body
  
  # need to parse issue
  # need to work this out
  
  #if (grepl("\\n\\n",body)) {
    #ss <- strsplit(body,"###") 
  #} else {
  #  ss <- strsplit(body,"\r\n\r\n")
    
  #}
  print(ss)
   
}
