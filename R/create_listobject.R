#' Reads in issues from GitHub and create rmds
#'
#' Some of the GitHub issues in catalog are tagged as "submission".
#' These issues are filtered and parsed then used to build catalog rmds.
#' Each rmd will represent a catalog index
#'
#' @param issuenum value. manually entered to select issue for parsing  (Default = NULL, all issues will be parsed)
#'
#' @return

create_listobject <- function(parsedIssue) {
  ### Generate listobject using gh_parser
  listobject <- list()
  listobject$dataname <- parsedIssue$`### Data Name (This will be the displayed title in Catalog)`
  listobject$indicatorname <- parsedIssue$`### Indicator Name (as exists in ecodata)`
  listobject$description <- parsedIssue$`### Data Description`

  ## Parse checkboxes
  family <- parsedIssue$`### Family (Which group is this indicator associated with?)`
  members <- unlist(stringr::str_extract_all(
    family,
    stringr::regex("- \\[X\\]\\s+[a-zA-z ,]+", ignore_case = T)
  ))
  members <- paste0(members, "\r\n", collapse = "")

  listobject$family <- members
  listobject$contributors <- parsedIssue$`### Data Contributors`
  listobject$affiliations <- parsedIssue$`### Affiliation`
  listobject$whatsthis <- parsedIssue$`### Introduction to Indicator (Please explain your indicator)`
  listobject$visualizations <- parsedIssue$`### Key Results and Visualization`
  listobject$indicatorStatsSpatial <- parsedIssue$`### Spatial Scale`
  listobject$indicatorStatsTemporal <- parsedIssue$`### Temporal Scale`

  ## Parse checkboxes
  synthesis <- parsedIssue$`### Synthesis Theme`
  members <- unlist(stringr::str_extract_all(
    synthesis,
    stringr::regex("- \\[X\\]\\s+[a-zA-z ,]+", ignore_case = T)
  ))
  members <- paste0(members, "\r\n", collapse = "")

  listobject$synthesisTheme <- members
  listobject$implications <- parsedIssue$`### Implications`
  listobject$poc <- parsedIssue$`### Point(s) of Contact`
  listobject$defineVariables <- parsedIssue$`### Define Variables`

  ## Parse checkboxes
  indicator <- parsedIssue$`### Indicator Category`
  members <- unlist(stringr::str_extract_all(
    indicator,
    stringr::regex("- \\[X\\]\\s+[a-zA-z ,]+", ignore_case = T)
  ))
  members <- paste0(members, "\r\n", collapse = "")

  listobject$indicatoryCategory <- members
  listobject$other <- parsedIssue$`### If other, please specify indicator category`

  listobject$publicAvailability <- parsedIssue$`### Public Availability`
  listobject$accessibility <- parsedIssue$`### Accessibility and Constraints`
  listobject$primaryContact <- parsedIssue$`### Primary Contact`
  listobject$secondaryContact <- parsedIssue$`### Secondary Contact`

  # Parse page status IF it exists
  if (!is.null(parsedIssue$`### Page Status`)) {
    listobject$page_status <- parsedIssue$`### Page Status`
  }

  # Add last issue updated to listobject
  listobject$last_updated <- parsedIssue$last_updated

  return(listobject)
}
