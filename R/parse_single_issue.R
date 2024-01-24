#' Parse a single Catalog submission issue from Github
#' 
#' @param issueData list. output from GitHub API pull of ALL issues
#' @param issueNum numeric. The number of the issue as assigned by GitHub
#' @param pull Boolean. Should pull directly from GitHub or use issueData
#' 
#' @return list of containing issue heading names with associated content
#' 

parse_single_issue <- function(issueData,issueNum,pull=F){
  
  objectList <- list()
  if (pull) {
    repo <- 'https://api.github.com/repos/NOAA-EDAB/catalog/issues'
    issue <- jsonlite::fromJSON(paste0(repo,"/",issueNum))
    body <- issue$body
  } else {
    id <- which(issueData$issues$number==issueNum)
    body <- issueData$issues[id,]$body
  }
  
  headings <- unlist(stringr::str_extract_all(body,"###\\s+[a-zA-Z (.)\"\\?,]+"))
  for (ahead in headings) {
    modifiedHead <- gsub("\\","",ahead,fixed=T)
    modifiedHead <- gsub("(","\\(",modifiedHead,fixed=T)
    modifiedHead <- gsub(")","\\)",modifiedHead,fixed=T)
    modifiedHead <- gsub("?","\\?",modifiedHead,fixed=T)
    byhead <- unlist(strsplit(body,modifiedHead))[2]
    res <- unlist(strsplit(byhead,"###"))[1]
    # remove beginning and trailing \n (line feed) and \r (carriage return)
    modifiedRes <- trimws(res)
    # modifiedRes <- sub("\\n+$","",modifiedRes)
    # modifiedRes <- sub("^\\r\\n+","",modifiedRes)
    # modifiedRes <- sub("\\r\\n+$","",modifiedRes)
    objectList[ahead] <- modifiedRes
  }
  
  return(objectList)
  
}