#' Github parser
#' 

parser <- function(issue = NULL){
  
  if (is.null(issue)) {
    # look through all issues
    # pulling all issue numnber
    # loop over issues
    for (iss in 1:nissues) {
      # search for submission tag
      # function to parse this issue
      listobject <- parse_issue(iss)
      # make rmd from list
      make_rmd(listobject)
      #inside make_rmd
      knitr::render_markdown("name of template rmd")
      # template rmd that is parameterized
      # read the list object param in rmd
      # saved with name in line 6 of list object
    }
    
    
  } else {
    # just this issue
    listobject <- parse_issue(iss)
    knitr::render_markdown("name of template rmd", param=listobject)
  }
  
  
  
  
}