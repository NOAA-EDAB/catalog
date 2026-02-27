#' Pulls all issues from GitHub API
#'
#' Issues that we are interested in have a "submission" tag associated with them.
#' This submission tag is assigned in GitHub to the the issue Template
#' so when a submission is entered it is automatically tagged
#'
#'
#' @return list object
#' \item{issueData}{a list of all issues . Each element is an issue}
#' \item{submissions}{the issue numbers that are "submissions"}

pull_all_issues <- function() {
  # Define repo information, pull issues from GH API and subset 'submissions'
  repo <- 'https://api.github.com/repos/NOAA-EDAB/catalog/issues'
  # pull all issues and select all submission issues
  issueData <- list()
  issueList <- list()
  issues <- NULL
  submissions <- NULL
  pulling <- TRUE
  page <- 1
  # pulls 100 issues
  while (pulling) {
    request_url <- paste0(repo, "?per_page=100&page=", page)
    # pull the data
    current_batch <- jsonlite::fromJSON(request_url)
    # Check if we got data back
    if (length(current_batch) == 0) {
      pulling <- FALSE
    } else {
      # make sure it has the "submission" tag associated with it
      indices <- which(unlist(lapply(current_batch$labels, function(x) {
        if (length(x$name) == 0) {
          F
        } else {
          x$name == "submission"
        }
      })))
      subs <- current_batch$number[indices]
      # store
      issues <- dplyr::bind_rows(issues, current_batch)
      issueList[[page]] <- current_batch

      submissions <- c(submissions, subs)
      page <- page + 1
    }
  }
  #issues <- do.call(dplyr::bind_rows, issueList)
  # save pull to rds for debugging
  issueData$issues <- issues
  issueData$submissions <- submissions
  saveRDS(issueData, here::here("data-raw/submissionIssueNumbers.rds"))

  return(issueData)
}
