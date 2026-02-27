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
      # Filter out anything that has a 'pull_request' element
      if ("pull_request" %in% names(current_batch)) {
        pure_issues_ind <- is.na(current_batch$pull_request$url)
        pure_issues <- current_batch[pure_issues_ind, ]
        pure_issues <- pure_issues |>
          dplyr::select(-pull_request, -draft)
      } else {
        pure_issues <- current_batch
      }

      print(dim(pure_issues))
      # make sure it has the "submission" tag associated with it
      indices <- which(unlist(lapply(pure_issues$labels, function(x) {
        if (length(x$name) == 0) {
          F
        } else {
          x$name == "submission"
        }
      })))
      subs <- pure_issues$number[indices]
      # store
      issues <- dplyr::bind_rows(issues, pure_issues)
      #issueList[[page]] <- pure_issues

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
