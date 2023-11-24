#' Parse GitHub issue into usable format
#' 
#' Process GitHub issues and convert into listobject used by make_Rmd function
#' 
#' @param issuenum value. manually entered to select issue for parsing
#' 
#' @return 

# Function below
gh_parser <- function(issuenum,linenum){
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