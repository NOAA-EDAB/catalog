#' Parse GitHub issue into usable format
#' 
#' Process GitHub issues and convert into listobject used by make_Rmd function
#' 
#' @param linenum value. designates which line of the issue template to draw from
#' 
#' @return 

# Function below
gh_parser <- function(linenum){
  if (grepl("\\n\\n",body_ul[linenum])) {
    body_rf <- unlist(strsplit(body_ul[linenum],"\n\\n")) 
  } else {
    body_rf <- unlist(strsplit(body_ul[linenum],"\r\n\r\n"))
  }
  
  return(toString(body_rf[-1]))
}