#' Access GitHub API 
#' 
#' process issues found on the repo
#' 
#' 

# Define repo information, pull issues from GH API and subset 'submissions'
repo <- 'https://api.github.com/repos/NOAA-EDAB/catalog/issues'
issues <- jsonlite::fromJSON(repo)
indices <- which(unlist(lapply(issues$labels,function(x) {if(length(x$name)==0){F}else{x$name=="submission"}})))
submissions <- issues$number[indices]

# Iterate over issues and generate named objects for each issue field
for (issuenum in submissions) {
  issue <- jsonlite::fromJSON(paste0(repo,"/",issuenum))
  body <- issue$body
  body_ss <- strsplit(body,"### ")
  body_ul <- unlist(body_ss)

# Split data name and create econame variable for naming conventions
  if (grepl("\\n\\n",body_ul[4])) {
    econame <- unlist(strsplit(body_ul[4],"\n\\n")) 
  } else {
    econame <- unlist(strsplit(body_ul[4],"\r\n\r\n"))
  }
  
# Iterate over each line of issue and create object for each field  
  for (linenum in 1:18) {
    if (grepl("\\n\\n",body_ul[linenum])) {
      body_rf <- unlist(strsplit(body_ul[linenum],"\n\\n")) 
    } else {
      body_rf <- unlist(strsplit(body_ul[linenum],"\r\n\r\n"))
    }
    
    assign(paste0(econame[-1],"_",body_rf[1]), toString(body_rf[-1]))
  }
}

### WRITE A FUNCTION THAT TAKES ISSUENUM, LINENUM AS VARIABLES AND PRODUCES LISTOBJECT
repo <- 'https://api.github.com/repos/NOAA-EDAB/catalog/issues'

gh_parser <- function(repo,issuenum,linenum){
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

### The gh_parser function should output the below listobject
listobject <- list()
listobject$indicatorname <- "more forage fish"
listobject$ecodataname <- "more_forage_fish"  # add to template (dropdown)
listobject$family <- "family" # add to template (dropdown)
listobject$description <- "a really long description"
listobject$contributors <- "contribs"
listobject$affiliations <- "affiliations" # add to template
listobject$whatsthis <- "what is this"
listobject$visualizations <- "visuals"
listobject$indicatorStatsSpatial <- "spatial"
listobject$indicatorStatsTemporal <- "temporal"
listobject$implications <- "implications"
listobject$poc <- "poc"

### gh_parser can be saved as a standalone function and used by another script/function, if needed