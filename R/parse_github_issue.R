#' Parse GitHub issue into usable format
#' 
#' Process GitHub issues and convert into listobject used by make_Rmd function
#' 
#' @param issuenum value. manually entered to select issue for parsing
#' 
#' @return 


# Define repo information, pull issues from GH API and subset 'submissions'
#repo <- 'https://api.github.com/repos/NOAA-EDAB/catalog/issues'
#issues <- jsonlite::fromJSON(repo)
#indices <- which(unlist(lapply(issues$labels,function(x) {if(length(x$name)==0){F}else{x$name=="submission"}})))
#submissions <- issues$number[indices]

# Inputs for testing
repo <- 'https://api.github.com/repos/NOAA-EDAB/catalog/issues' # Manually select repo (for now)
issuenum <- 23 # Manually select repo for function. Initial catalog build will use loop

# Function below
gh_parser <- function(issuenum,linenum){
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
listobject$indicatorname <- gh_parser(issuenum,4)
listobject$ecodataname <- "more_forage_fish"  # add to template (dropdown)
listobject$family <- "family" # add to template (dropdown)
listobject$description <- gh_parser(issuenum,5)
listobject$contributors <- gh_parser(issuenum,15)
listobject$affiliations <- "affiliations" # add to template
listobject$whatsthis <- gh_parser(issuenum,6)
listobject$visualizations <- gh_parser(issuenum,7)
listobject$indicatorStatsSpatial <- gh_parser(issuenum,9)
listobject$indicatorStatsTemporal <- gh_parser(issuenum,10)
listobject$implications <- gh_parser(issuenum,8)
listobject$poc <- gh_parser(issuenum,16)

### Test make_Rmd.R using above inputs
make_rmd(listobject)