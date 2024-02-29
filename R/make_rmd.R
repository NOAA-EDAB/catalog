#' Create markdown file using parsed github issue
#' 
#' Template structure for the rmd is hard coded in this function. 
#' The template used data from github parsed github submission issue
#' 
#' @param listobject list. parsed issue from github
#' 
#' @return creates an rmd in chapters folder. The name of the rmd is the indicator name

make_rmd <- function(listobject){
  
  # line for the indicator name
  #indicator_name <- listobject$name
  # create filename based on indicator name. Replace all spaces with underscore
  #filename <- gsub("\\s+","_",indicator_name)
  filename <- listobject$indicatorname
  
  # create rmd with name of indicator
  con <- file(here::here("chapters",paste0(filename,".rmd")),open="w")
     
  # start to create the Rmd
  #cat(paste0("# ",stringr::str_to_title(indicator_name)),append=T,fill=T,file=con)  
  ### DESCRIPTION, CONTRIBUTORS, AFFILIATION, FAMILY
  cat(paste0("# ",listobject$dataname," {#",listobject$indicatorname,"}"),append=T,fill=T,file=con)    
  #cat(paste0("# ",listobject$dataname),append=T,fill=T,file=con)    
  cat("",append=T,fill=T,file=con) # add space
  cat(paste0("**Description**: ",listobject$description),append=T,fill=T,file=con)
  cat("",append=T,fill=T,file=con) # add space
  cat("**Indicator family**: ",append=T,fill=T,file=con)
  cat("",append=T,fill=T,file=con) # add space
  cat(listobject$family,append=T,fill=T,file=con)
  cat("",append=T,fill=T,file=con) # add space
  cat(paste0("**Contributor(s)**: ",listobject$contributors),append=T,fill=T,file=con)
  cat("",append=T,fill=T,file=con) # add space
  cat(paste0("**Affiliations**: ",listobject$affiliations),append=T,fill=T,file=con)
  cat("",append=T,fill=T,file=con) # add space
  
  # chunk script
  cat("```{r echo=FALSE}",append=T,fill=T,file=con)
  cat("knitr::opts_chunk$set(echo = F)",append=T,fill=T,file=con)
  cat("library(ecodata)",append=T,fill=T,file=con)
  cat("```",append=T,fill=T,file=con)
  
  ### DESCRIPTION OF INDICATOR
  cat("## Introduction to Indicator",append=T,fill=T,file=con)
  cat(listobject$whatsthis,append=T,fill=T,file=con)
  cat("",append=T,fill=T,file=con) # add space
  
  ### PLOT INDICATOR
  cat("## Key Results and Visualizations",append=T,fill=T,file=con)
  cat(listobject$visualizations,append=T,fill=T,file=con)
  cat("",append=T,fill=T,file=con) # add space
  
  ### rchunk code to run plot functions from ecodata
  # gets a bit messy trying to check for function arguments
  # Find the names of the arguments to the function
  
  if (exists(paste0("plot_",listobject$indicatorname))) {
  
    at <- attributes(eval(parse(text=paste0("ecodata::plot_",listobject$indicatorname))))
    
    functionArgs <- names(formals(eval(parse(text=paste0("ecodata::plot_",listobject$indicatorname)))))

    if (length(functionArgs) == 2 | ((length(functionArgs) == 3) & (any(c("year","scale") %in% functionArgs)))) {
      # this is standard shadedRegion and report
      # check to see how many EPUs are listed in data object and/or they are "All" (shelfwide)
      indicatorData <- eval(parse(text=paste0("ecodata::",listobject$indicatorname)))  
      epus <- unique(indicatorData$EPU)
      if (any(c("all","na") %in% tolower(epus)) | is.null(epus)) {
        # plot just one map
        cat(paste0("```{r plot_",listobject$indicatorname,"MAB}"),append=T,fill=T,file=con)
        cat("# Plot indicator",append=T,fill=T,file=con)
        cat(paste0("ggplotObject <- ecodata::plot_",listobject$indicatorname,"(report='MidAtlantic')"),append=T,fill=T,file=con)
        cat("ggplotObject",append=T,fill=T,file=con)
        cat("```",append=T,fill=T,file=con)
        cat("",append=T,fill=T,file=con) # add space
      } else {
        # stock_status has no EPU. non conforming data set
        if ("mab" %in% tolower(epus)) {
          # plot MidAtlantic Report 
          cat("### MAB",append=T,fill=T,file=con)
          cat("",append=T,fill=T,file=con) # add space
          cat(paste0("```{r plot_",listobject$indicatorname,"MAB}"),append=T,fill=T,file=con)
          cat("# Plot indicator",append=T,fill=T,file=con)
          cat(paste0("ggplotObject <- ecodata::plot_",listobject$indicatorname,"(report='MidAtlantic')"),append=T,fill=T,file=con)
          cat("ggplotObject",append=T,fill=T,file=con)
          cat("```",append=T,fill=T,file=con)
          cat("",append=T,fill=T,file=con) # add space
        } 
        if (any(c("gb","gom","ne") %in% tolower(epus))) {
        # plot NewEngland Report
          cat("### NE",append=T,fill=T,file=con)
          cat("",append=T,fill=T,file=con) # add space
          cat(paste0("```{r plot_",listobject$indicatorname,"NE}"),append=T,fill=T,file=con)
          cat("# Plot indicator",append=T,fill=T,file=con)
          cat(paste0("ggplotObject <- ecodata::plot_",listobject$indicatorname,"(report='NewEngland')"),append=T,fill=T,file=con)
          cat("ggplotObject",append=T,fill=T,file=con)
          cat("```",append=T,fill=T,file=con)
          cat("",append=T,fill=T,file=con) # add space
        }
      }
  
    } else if (length(functionArgs) == 3 & ("EPU" %in% functionArgs )) {
      # additional EPU argument. Create 3 plots, one for each EPU
      cat("### MAB",append=T,fill=T,file=con)
      cat("",append=T,fill=T,file=con) # add space
      cat(paste0("```{r plot_",listobject$indicatorname,"MAB}"),append=T,fill=T,file=con)
      cat("# Plot indicator",append=T,fill=T,file=con)
      cat(paste0("ggplotObject <- ecodata::plot_",listobject$indicatorname,"(report='MidAtlantic')"),append=T,fill=T,file=con)
      cat("ggplotObject",append=T,fill=T,file=con)
      cat("```",append=T,fill=T,file=con)
      cat("",append=T,fill=T,file=con) # add space
      
      ## GB
      cat("### GB",append=T,fill=T,file=con)
      cat("",append=T,fill=T,file=con) # add space
      cat(paste0("```{r plot_",listobject$indicatorname,"NEGB}"),append=T,fill=T,file=con)
      cat("# Plot indicator",append=T,fill=T,file=con)
      cat(paste0("ggplotObject <- ecodata::plot_",listobject$indicatorname,"(report='NewEngland',EPU='GB')"),append=T,fill=T,file=con)
      cat("ggplotObject",append=T,fill=T,file=con)
      cat("```",append=T,fill=T,file=con)
      cat("",append=T,fill=T,file=con) # add space
      
      ## GOM
      cat("### GOM",append=T,fill=T,file=con)
      cat("",append=T,fill=T,file=con) # add space
      cat(paste0("```{r plot_",listobject$indicatorname,"NEGOM}"),append=T,fill=T,file=con)
      cat("# Plot indicator",append=T,fill=T,file=con)
      cat(paste0("ggplotObject <- ecodata::plot_",listobject$indicatorname,"(report='NewEngland',EPU='GOM')"),append=T,fill=T,file=con)
      cat("ggplotObject",append=T,fill=T,file=con)
      cat("```",append=T,fill=T,file=con)
      cat("",append=T,fill=T,file=con) # add space
      
          
    } 
    
    if (length(at) == 3 & ("varName" %in% names(at)) ){
      # make all plots of varName and report
      for (arep in at$report) {
        cat(paste0("### ",arep),append=T,fill=T,file=con)
        cat("",append=T,fill=T,file=con) # add space
        for (avar in at$varName) {
          avarws <- gsub("\\s", "", avar)  
          # create varName plots
          cat(paste0("```{r plot_",listobject$indicatorname,arep,avarws,"}"),append=T,fill=T,file=con)
          cat("# Plot indicator",append=T,fill=T,file=con)
          cat(paste0("ggplotObject <- ecodata::plot_",listobject$indicatorname,"(report= '",arep,"', varName= '",avar,"')"),append=T,fill=T,file=con)
          cat("ggplotObject",append=T,fill=T,file=con)
          cat("```",append=T,fill=T,file=con)
          cat("",append=T,fill=T,file=con) # add space
        }
      }
      
    }
    
    if (length(at) == 3 & ("plottype" %in% names(at)) ){
      # make all plots of plottype and report
      for (arep in at$report) {
        cat(paste0("### ",arep),append=T,fill=T,file=con)
        cat("",append=T,fill=T,file=con) # add space
        for (avar in at$plottype) {
          avarws <- gsub("\\s", "", avar)  # remov whitespace from name
          # create varName plots
          cat(paste0("```{r plot_",listobject$indicatorname,arep,avarws,"}"),append=T,fill=T,file=con)
          cat("# Plot indicator",append=T,fill=T,file=con)
          cat(paste0("ggplotObject <- ecodata::plot_",listobject$indicatorname,"(report= '",arep,"', plottype= '",avar,"')"),append=T,fill=T,file=con)
          cat("ggplotObject",append=T,fill=T,file=con)
          cat("```",append=T,fill=T,file=con)
          cat("",append=T,fill=T,file=con) # add space
     
        }
      }
      
    }
  
    if (length(at) == 4 & ("varName" %in% names(at)) ){
      newV <- names(at)[which(!(names(at) %in% c("varName","srcref","report")))]
      newVals <- at[[newV]]
      # make all plots of plottype and report
      for (arep in at$report) {
        cat(paste0("### ",arep),append=T,fill=T,file=con)
        cat("",append=T,fill=T,file=con) # add space
        
        for (avar in at$varName) {
        
          if (tolower(newV) == "epu") {
            if (arep == "MidAtlantic") {
              newVals <- "MAB"
            } else {
              newVals <- c("GB","GOM")
            }
          }
          
          for (aval in newVals) {
            avarws <- gsub("\\s", "", avar)  # remov whitespace from name
            # create varName plots
            cat(paste0("```{r plot_",listobject$indicatorname,arep,avarws,aval,"}"),append=T,fill=T,file=con)
            cat("# Plot indicator",append=T,fill=T,file=con)
            cat(paste0("ggplotObject <- ecodata::plot_",listobject$indicatorname,"(report= '",arep,"', varName= '",avar,"' ,",newV,"= '",aval,"')"),append=T,fill=T,file=con)
            cat("ggplotObject",append=T,fill=T,file=con)
            cat("```",append=T,fill=T,file=con)
            cat("",append=T,fill=T,file=con) # add space
            
          }
        }
      }
    }
    
    if (length(at) > 4 & (all(c("varName","plottype") %in% names(at))) ) {
        # make all plots of plottype and report
        for (arep in at$report) {
          cat(paste0("### ",arep),append=T,fill=T,file=con)
          cat("",append=T,fill=T,file=con) # add space
          for (avar in at$varName) {
            for (atype in at$plottype) {
              avarws <- gsub("\\s", "", avar)  # remov whitespace from name
              # create varName plots
              cat(paste0("```{r plot_",listobject$indicatorname,arep,avarws,atype,"}"),append=T,fill=T,file=con)
              cat("# Plot indicator",append=T,fill=T,file=con)
              cat(paste0("ggplotObject <- ecodata::plot_",listobject$indicatorname,"(report= '",arep,"', varName= '",avar,"', plottype = '",atype,"')"),append=T,fill=T,file=con)
              cat("ggplotObject",append=T,fill=T,file=con)
              cat("```",append=T,fill=T,file=con)
              cat("",append=T,fill=T,file=con) # add space
              
            }
          }
        }
      }
  }
  
  
  ### SPATIAL + TEMPORAL SCALE
  cat("",append=T,fill=T,file=con) # add space
  cat("## Indicator statistics ",append=T,fill=T,file=con)
  cat(paste0("Spatial scale: ",listobject$indicatorStatsSpatial),append=T,fill=T,file=con)
  cat("",append=T,fill=T,file=con) # add space
  cat(paste0("Temporal scale: ",listobject$indicatorStatsTemporal),append=T,fill=T,file=con)
  cat("",append=T,fill=T,file=con) # add space
  
  ### SYNTHESIS THEME
  cat("**Synthesis Theme**:",append=T,fill=T,file=con)
  cat("",append=T,fill=T,file=con) # add space
  cat(listobject$synthesisTheme,append=T,fill=T,file=con)
  cat("",append=T,fill=T,file=con) # add space
  
  # r chunk for autostats
  cat(paste0("```{r autostats_",filename,"}"),append=T,fill=T,file=con)
  cat("# Either from Contributor or ecodata",append=T,fill=T,file=con)
  cat("```",append=T,fill=T,file=con)
  cat("",append=T,fill=T,file=con) # add space
  
  ### IMPLICATIONS 
  cat("## Implications",append=T,fill=T,file=con)
  cat(listobject$implications,append=T,fill=T,file=con)
  cat("",append=T,fill=T,file=con) # add space
  
  
  cat("## Get the data",append=T,fill=T,file=con)
  cat("",append=T,fill=T,file=con) # add space
  
  ### CONTACT PERSON
  cat(paste0("**Point of contact**: [",listobject$poc,"](mailto:",listobject$poc,"){.email}"),append=T,fill=T,file=con)
  cat("",append=T,fill=T,file=con) # add space
  
  if (exists(paste0(listobject$indicatorname))) {
    cat(paste0("**ecodata name**: `ecodata::",listobject$indicatorname,"`"),append=T,fill=T,file=con)
    cat("",append=T,fill=T,file=con) # add space
  } else {
    cat(paste0("**ecodata name**: No dataset"),append=T,fill=T,file=con)
    cat("",append=T,fill=T,file=con)
  }
  ### VARIABLES FOUND IN DATA
  cat("**Variable definitions**",append=T,fill=T,file=con)
  cat("",append=T,fill=T,file=con) # add space

  # grabs the defined variables and writes out as an unordered list
  cat(unlist(strsplit(listobject$defineVariables,"\\n")),append=T,fill=T,file=con)
  cat("",append=T,fill=T,file=con) # add space

  # check to see if data name exists. Some do not but need a catalog page
  if (exists(paste0(listobject$indicatorname))) {  
    # check to see if Var field exists
    if ("Var" %in% names(eval(parse(text=paste0("ecodata::",listobject$indicatorname))))) {
      # r code chunk to make table
      cat(paste0("```{r vars_",listobject$indicatorname,"}"),append=T,fill=T,file=con)
      cat("# Pull all var names",append=T,fill=T,file=con)
    
      cat(paste0("vars <- ecodata::",listobject$indicatorname," |>"),append=T,fill=T,file=con)
      if (eval(parse(text=paste0("'Units' %in% names(ecodata::",listobject$indicatorname,")")))) {
        cat("   dplyr::select(Var, Units) |>",append=T,fill=T,file=con)
      } else {
        cat("   dplyr::select(Var) |>",append=T,fill=T,file=con)
      }
      cat("   dplyr::distinct()",append=T,fill=T,file=con)
      cat("",append=T,fill=T,file=con) # add space
      cat("DT::datatable(vars)",append=T,fill=T,file=con) # add space
      cat("```",append=T,fill=T,file=con)
    }
  } else {
    cat("",append=T,fill=T,file=con) # add space
    cat("No Data",append=T,fill=T,file=con) # add space
    cat("",append=T,fill=T,file=con) # add space
  }
  
  ### INDICATOR CATEGORY
  cat("**Indicator Category**:",append=T,fill=T,file=con)
  cat("",append=T,fill=T,file=con) # add space
  cat(listobject$indicatoryCategory,append=T,fill=T,file=con)
  cat("",append=T,fill=T,file=con) # add space
  
  if(!(listobject$other == "_No response_")) {
    cat("**Indicator Category**:",append=T,fill=T,file=con)
    cat("",append=T,fill=T,file=con) # add space
    cat(listobject$other,append=T,fill=T,file=con)
    cat("",append=T,fill=T,file=con) # add space
  }
  
  ### PUBLIC AVAILABILITY + ACCESSIBILITY
  
  cat("## Public Availability",append=T,fill=T,file=con)
  cat("",append=T,fill=T,file=con) # add space
  cat(listobject$publicAvailability,append=T,fill=T,file=con)
  cat("",append=T,fill=T,file=con) # add space
  
  cat("## Accessibility and Constraints",append=T,fill=T,file=con)
  cat("",append=T,fill=T,file=con) # add space
  cat(listobject$accessibility,append=T,fill=T,file=con)
  cat("",append=T,fill=T,file=con) # add space
  

  # write catalog link if ecodata data is present
  if (exists(paste0(listobject$indicatorname))) {
    cat("**tech-doc link**",append=T,fill=T,file=con)
    
    cat(paste0("<https://noaa-edab.github.io/tech-doc/",listobject$indicatorname,".html>"),append=T,fill=T,file=con)
    cat("",append=T,fill=T,file=con) # add space
    
  } else {
    # No link since this is a synthesis type of page
  }
  
  # References are generated automatically  

  
  #close the connection
  close(con)
  
 
}