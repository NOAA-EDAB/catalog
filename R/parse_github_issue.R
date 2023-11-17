#' Access GitHub API 
#' 
#' process issues found on the repo
#' 
#' 

# Define repo information, pull issues from GH API and subset 'submissions'
#repo <- 'https://api.github.com/repos/NOAA-EDAB/catalog/issues'
#issues <- jsonlite::fromJSON(repo)
#indices <- which(unlist(lapply(issues$labels,function(x) {if(length(x$name)==0){F}else{x$name=="submission"}})))
#submissions <- issues$number[indices]

### Everything below should be the parsing script
### WRITE A FUNCTION THAT TAKES ISSUENUM, LINENUM AS VARIABLES AND PRODUCES LISTOBJECT
repo <- 'https://api.github.com/repos/NOAA-EDAB/catalog/issues' # Manually select repo (for now)
issuenum <- 23 # Manually select repo for function. Initial catalog build will use loop

gh_parser <- function(linenum){
  issue <- jsonlite::fromJSON(paste0(repo,"/",issuenum))
  body <- issue$body
  body_ss <- strsplit(body,"### ")
  body_ul <- unlist(body_ss)
  
  if (grepl("\\n\\n",body_ul[linenum])) {
    body_rf <- unlist(strsplit(body_ul[linenum],"\n\\n")) 
  } else {
    body_rf <- unlist(strsplit(body_ul[linenum],"\r\n\r\n"))
  }
  
  return(toString(body_rf[-1]))
}

### Generate listobject using gh_parser
listobject <- list()
listobject$indicatorname <- gh_parser(4)
listobject$ecodataname <- "more_forage_fish"  # add to template (dropdown)
listobject$family <- "family" # add to template (dropdown)
listobject$description <- gh_parser(5)
listobject$contributors <- gh_parser(15)
listobject$affiliations <- "affiliations" # add to template
listobject$whatsthis <- gh_parser(6)
listobject$visualizations <- gh_parser(7)
listobject$indicatorStatsSpatial <- gh_parser(9)
listobject$indicatorStatsTemporal <- gh_parser(10)
listobject$implications <- gh_parser(8)
listobject$poc <- gh_parser(16)

### Test make_Rmd.R using above inputs
make_rmd(listobject)
