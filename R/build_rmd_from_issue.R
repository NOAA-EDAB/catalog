#' Reads in issues from GitHub and create rmds
#' 
#' Some of the GitHub issues in catalog are tagged as "submission".
#' These issues are filtered and parsed then used to build catalog rmds.
#' Each rmd will represent a catalog index
#' 
#' @param issuenum value. manually entered to select issue for parsing  (Default = NULL, all issues will be parsed)
#' 
#' @return 

# Source required functions
source(here::here("R/parse_single_issue.R"))
source(here::here("R/make_rmd.R"))

build_rmd_from_issue <- function(issuenum = 1) {

  # Define repo information, pull issues from GH API and subset 'submissions'
  repo <- 'https://api.github.com/repos/NOAA-EDAB/catalog/issues'
  issues <- jsonlite::fromJSON(repo)
  indices <- which(unlist(lapply(issues$labels,function(x) {if(length(x$name)==0){F}else{x$name=="submission"}})))
  submissions <- issues$number[indices]
  
  if (!is.null(issuenum)) {
    if (issuenum %in% submissions) {
      submissions <- issuenum
    } else {
      stop("This issue number is not a submission issue and can not be parsed")
    }
  } 
  
  
  for (issuenum in submissions) {
    message(paste0("Issue number: ",issuenum))
    ### Pull issue from GitHub API
    parsedIssue <- parse_single_issue(repo,issuenum)
    
    
    ### Generate listobject using gh_parser
    listobject <- list()
    listobject$dataname <- parsedIssue$`### Data Name (This will be the displayed title in Catalog)`
    listobject$indicatorname <- parsedIssue$`### Indicator Name (as exists in ecodata)`
    listobject$description <- parsedIssue$`### Data Description`
    listobject$family <- parsedIssue$`### Family (Which group is this indicator associated with?)`
    listobject$contributors <- parsedIssue$`### Data Contributors`
    listobject$affiliations <- parsedIssue$`### Affiliation`
    listobject$whatsthis <- parsedIssue$`### Introduction to Indicator (Please explain your indicator)`
    listobject$visualizations <- parsedIssue$`### Key Results and Visualization`
    listobject$indicatorStatsSpatial <- parsedIssue$`### Spatial Scale`
    listobject$indicatorStatsTemporal <- parsedIssue$`### Temporal Scale`
    listobject$synthesisTheme <- parsedIssue$`### Synthesis Theme`
    listobject$implications <- parsedIssue$`### Implications`
    listobject$poc <- parsedIssue$`### Point(s) of Contact`
    listobject$defineVariables <- parsedIssue$`### Define Variables`
    listobject$indicatoryCategory <- parsedIssue$`### Indicator Category`
    listobject$other <- parsedIssue$`### If other, please specify indicator category`
  
    listobject$publicAvailability <- parsedIssue$`### Public Availability`
    listobject$accessibility <- parsedIssue$`### Accessibility and Constraints`
    listobject$primaryContact <- parsedIssue$`### Primary Contact`
    listobject$secondaryContact <- parsedIssue$`### Secondary Contact`
    
  
  ### Test make_Rmd.R using above inputs
  make_rmd(listobject)
  }

}