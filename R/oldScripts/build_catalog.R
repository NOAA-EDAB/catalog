#' Build catalog using make_rmd and gh_parser functions
#' 
#' Process GitHub issues and convert into listobject used by make_Rmd function
#' 
#' @param issuenum value. manually entered to select issue for parsing
#' 
#' @return 

# Source required functions
source(here::here("R/parse_github_issue.R"))
source(here::here("R/make_rmd.R"))

# Define repo information, pull issues from GH API and subset 'submissions'
repo <- 'https://api.github.com/repos/NOAA-EDAB/catalog/issues'
issues <- jsonlite::fromJSON(repo)
indices <- which(unlist(lapply(issues$labels,function(x) {if(length(x$name)==0){F}else{x$name=="submission"}})))
submissions <- issues$number[indices]

# Manual issue selection
#issuenum <- 37 # Manually select repo for function. Initial catalog build will use loop

for (issuenum in submissions) {
### Pull issue from GitHub API
issue <- jsonlite::fromJSON(paste0(repo,"/",issuenum))
body <- issue$body
body_ss <- strsplit(body,"### ")
body_ul <- unlist(body_ss)
  
### Generate listobject using gh_parser
listobject <- list()
listobject$indicatorname <- gh_parser(4)
listobject$ecodataname <- gh_parser(4)
listobject$family <- "family"
listobject$description <- gh_parser(5)
listobject$contributors <- gh_parser(15)
listobject$affiliations <- "affiliations"
listobject$whatsthis <- gh_parser(6)
listobject$visualizations <- gh_parser(7)
listobject$indicatorStatsSpatial <- gh_parser(9)
listobject$indicatorStatsTemporal <- gh_parser(10)
listobject$implications <- gh_parser(8)
listobject$poc <- gh_parser(16)

### Test make_Rmd.R using above inputs
make_rmd(listobject)
}