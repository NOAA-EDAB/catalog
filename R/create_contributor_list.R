#' Read in submissions and create a contrib list
#'
#' REquires user to have already pulled all issues
#'

source(here::here("R/parse_issue.R"))
source(here::here("R/create_listobject.R"))

create_contributor_list <- function() {
  if (file.exists(here::here("data-raw/submissionIssueNumbers.rds"))) {
    contacts <- list()
    # read in all issues
    issueData <- readRDS(here::here("data-raw/submissionIssueNumbers.rds"))
    # select all submission issues
    submissions <- issueData$submissions
    # Process the issues
    for (issuenum in submissions) {
      message(paste0("Issue number: ", issuenum))
      ### Parse issue
      parsedIssue <- parse_issue(issueData, issuenum)
      # create list of parsed issue values
      listobject <- create_listobject(parsedIssue)

      # create list of contact info. pick out selceted fields
      contacts[[listobject$indicatorname]]$indicator <- listobject$indicatorname
      contacts[[listobject$indicatorname]]$contributors <- gsub(
        ",",
        ";",
        listobject$contributors
      )
      contacts[[listobject$indicatorname]]$poc <- gsub(",", ";", listobject$poc)
      contacts[[listobject$indicatorname]]$poc2 <- gsub(
        ",",
        ";",
        listobject$secondaryContact
      )
      contacts[[listobject$indicatorname]]$affiliations <- gsub(
        ",",
        ";",
        listobject$affiliations
      )
    }

    # write to a file
    file <- here::here("R/contribList.csv")
    c2 <- NULL
    # loop through each indicator and create a data frame
    for (aname in names(contacts)) {
      d <- as.data.frame(contacts[[aname]])
      c2 <- rbind(c2, d)
    }

    # write to file
    readr::write_csv(c2, file = file)

    message(" 'R/contribList.csv' contains list of contributor info ")
  } else {
    message("The file requried doesn't exist locally. You'll need to create it")
    message(" Please run build_rmd_from_issue(pullAllIssues=T)")
  }
}
