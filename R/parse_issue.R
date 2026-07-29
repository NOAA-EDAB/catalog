#' Parse a single Catalog submission issue from Github
#'
#' @param issueData list. output from GitHub API pull of ALL issues
#' @param issueNum numeric. The number of the issue as assigned by GitHub
#'
#' @return list of containing issue heading names with associated content
#'

parse_issue <- function(issueData, issueNum) {
  objectList <- list()
  if (length(issueData$issues$number) == 1) {
    # single issue
    body <- issueData$issues$body
    last_updated <- issueData$issues$updated_at # Parse the last time the issue was modified
  } else {
    # multiple issues. pick the right one
    id <- which(issueData$issues$number == issueNum)
    body <- issueData$issues[id, ]$body
    last_updated <- issueData$issues[id, ]$updated_at # Parse the last time the issue was modified
  }

  headings <- unlist(stringr::str_extract_all(
    body,
    "\n###\\s+[a-zA-Z (.)\"\\?,]+"
  ))

  for (ahead in headings) {
    modifiedHead <- gsub("\\", "", ahead, fixed = T)
    modifiedHead <- gsub("(", "\\(", modifiedHead, fixed = T)
    modifiedHead <- gsub(")", "\\)", modifiedHead, fixed = T)
    modifiedHead <- gsub("?", "\\?", modifiedHead, fixed = T)
    byhead <- unlist(strsplit(body, modifiedHead))[2]
    res <- unlist(strsplit(byhead, "\n### "))[1]
    # remove beginning and trailing \n (line feed) and \r (carriage return)
    modifiedRes <- trimws(res)
    # modifiedRes <- sub("\\n+$","",modifiedRes)
    # modifiedRes <- sub("^\\r\\n+","",modifiedRes)
    # modifiedRes <- sub("\\r\\n+$","",modifiedRes)
    ahead <- trimws(ahead)
    objectList[ahead] <- modifiedRes
  }

  # Add last issue update time to object list
  objectList$last_updated <- last_updated
  objectList$last_updated <- as.Date(objectList$last_updated) # Format as a date
  objectList$last_updated <- format(objectList$last_updated, "%B %d, %Y") # FOrmat as Month Day, Year

  return(objectList)
}
