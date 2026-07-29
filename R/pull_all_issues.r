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
  # Configure API pull
  repo <- 'https://api.github.com/repos/NOAA-EDAB/catalog/issues' # Link to target repo
  issues <- list() # Initialize empty list object
  per_page <- 100 # Determine issues per page (max 100)
  page <- 1 # Initialize page object

  repeat {
    # Define target URL using page specifications
    repopull <- paste0(repo, "?per_page=", per_page, "&page=", page)

    # Pull issues from current page
    # Each loop will pull from the next page
    current_page_issues <- jsonlite::fromJSON(repopull)

    # End the loop when page contains 0 issues
    if (length(current_page_issues) == 0) {
      break
    }

    # Store issues from current page as list
    issues <- dplyr::bind_rows(issues, current_page_issues)

    # Increment page number
    page <- page + 1
  }

  # Find issue numbers labeled "submission"
  indices <- which(unlist(lapply(issues$labels, function(x) {
    if (length(x$name) == 0) {
      F
    } else {
      x$name == "submission"
    }
  })))

  # Subset issues labeled "submission"
  submissions <- issues$number[indices]

  # Initialize issueData object to store API pull
  issueData <- list()

  # Save all issue data
  issueData$issues <- issues

  # Save "submission" issue data
  issueData$submissions <- submissions

  # Save pull data as rds for debugging
  saveRDS(issueData, here::here("data-raw/submissionIssueNumbers.rds"))

  return(issueData)
}
