# fmt: skip file
#' Create markdown file using parsed github issue
#' 
#' Template structure for the rmd is hard coded in this function. 
#' The template used data from github parsed github submission issue
#' 
#' @param listobject list. parsed issue from github
#' @param n numeric. Number of data points for short term trend
#' 
#' @return creates an rmd in chapters folder. The name of the rmd is the indicator name

make_rmd <- function(listobject, n = 10){
  
  # line for the indicator name
  #indicator_name <- listobject$name
  # create filename based on indicator name. Replace all spaces with underscore
  #filename <- gsub("\\s+","_",indicator_name)
  filename <- listobject$indicatorname
  
  # Define page type based on ecodata presence
  if (exists(paste0(listobject$indicatorname))) {
    page_type <- "ecodata"
  } else {
    page_type <- "independent"
  }

  # Assign spatial subtype to prevent extremely downloads
  if (listobject$indicatorname %in% c("bottom_temp_model_gridded", "thermal_habitat_gridded", "ches_bay_sst")) {
    page_subtype <- "spatial"
  } else {
    page_subtype <- "standard"
  }

  # create rmd with name of indicator
  con <- file(here::here("chapters",paste0(filename,".rmd")),open="w")
     
  # start to create the Rmd
  ### PAGE TITLE
  cat(paste0("# ",listobject$dataname," {#",listobject$indicatorname,"}"),append=T,fill=T,file=con)    
  cat("",append=T,fill=T,file=con) # add space

  ### Chunk to load packages
  cat("```{r echo=FALSE}",append=T,fill=T,file=con)
  cat("knitr::opts_chunk$set(echo = F)",append=T,fill=T,file=con)
  cat("library(ecodata)",append=T,fill=T,file=con)
  cat("library(downloadthis)",append=T,fill=T,file=con)
  cat("```",append=T,fill=T,file=con)
  cat("",append=T,fill=T,file=con) # add space

  ### Create status badge, if applicable
  if (!is.null(listobject$page_status)) {
    if (listobject$page_status == "Under Review"){
      cat(paste0('<a href="https://bbeltz1.github.io/catalogv2/page_status.html#under-review" class="btn btn-danger button_small"><i class="fa fa-warning"></i> Under Review</a>'),append=T,fill=T,file=con)
      cat("",append=T,fill=T,file=con) # add space

    } else if (listobject$page_status == "Under Construction") {
        cat(paste0('<a href="https://bbeltz1.github.io/catalogv2/page_status.html#under-construction" class="btn btn-warning button_small"><i class="fas fa-hard-hat"></i> Under Construction</a>'),append=T,fill=T,file=con)
        cat("",append=T,fill=T,file=con) # add space

    } else if (listobject$page_status == "In Development") {
        cat(paste0('<a href="https://bbeltz1.github.io/catalogv2/page_status.html#in-development" class="btn btn-success button_small"><i class="fas fa-pencil-ruler"></i> In Development</a>'),append=T,fill=T,file=con)
        cat("",append=T,fill=T,file=con) # add space
    }
  }

  ### ISSUE LAST MODIFIED
  cat(paste0("*Last updated ",listobject$last_updated,"*"),append=T,fill=T,file=con)
  cat("",append=T,fill=T,file=con) # add space

  ### DESCRIPTION, CONTRIBUTORS, AFFILIATION
  cat(paste0("**Description**: ",listobject$description),append=T,fill=T,file=con)
  cat("",append=T,fill=T,file=con) # add space
  cat(paste0("**Contributor(s)**: ",listobject$contributors),append=T,fill=T,file=con)
  cat("",append=T,fill=T,file=con) # add space
  cat(paste0("**Affiliations**: ",listobject$affiliations),append=T,fill=T,file=con)
  cat("",append=T,fill=T,file=con) # add space

  ### INDICATOR FAMILY
  cat("**Indicator Family**: ",append=T,fill=T,file=con)
  cat("",append=T,fill=T,file=con) # add space
  cat(listobject$family,append=T,fill=T,file=con)
  cat("",append=T,fill=T,file=con) # add space
    
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

  ### SYNTHESIS THEME
  cat("**Synthesis Theme**:",append=T,fill=T,file=con)
  cat("",append=T,fill=T,file=con) # add space
  cat(listobject$synthesisTheme,append=T,fill=T,file=con)
  cat("",append=T,fill=T,file=con) # add space
   
  ### Download dataset button if page type is ecodata but not spatial
  if (page_type == "ecodata" & page_subtype != "spatial"){
    # Open and name code chunk
    cat(paste0("```{r download_", listobject$indicatorname, "}"),append=T,fill=T,file=con)
    # Write header to .Rmd  
    cat("# Download dataset from ecodata",append=T,fill=T,file=con)
    # Call download_this function
    cat("try(downloadthis::download_this(",append=T,fill=T,file=con)
    # Paste ecodata dataset name
    cat(paste0("ecodata::", listobject$indicatorname, ","),append=T,fill=T,file=con)
    # Name output file using ecodata dataset name
    cat(paste0("output_name = '", listobject$indicatorname, "',"),append=T,fill=T,file=con)
    # Set output extension to .csv
    cat("output_extension = '.csv',",append=T,fill=T,file=con)
    # Define button label
    cat("button_label = 'Download dataset from `ecodata`',",append=T,fill=T,file=con)
    # Define button type
    cat("button_type = 'default',",append=T,fill=T,file=con)
    # Toggle on icon
    cat("has_icon = TRUE,",append=T,fill=T,file=con)
    # Define which icon to use
    cat("icon = 'fa fa-save',",append=T,fill=T,file=con)
    # Define button class
    cat("class = 'hvr-sweep-to-left',",append=T,fill=T,file=con)
    # Set output mode to csv2
    cat("csv2 = F),",append=T,fill=T,file=con)
    ## Suppress warnings and errors
    cat("silent = TRUE)",append=T,fill=T,file=con)
    # Close code chunk
    cat("```",append=T,fill=T,file=con)
    # Add space after code chunk
    cat("",append=T,fill=T,file=con)
  }

  ### DESCRIPTION OF INDICATOR
  cat("## Introduction to Indicator",append=T,fill=T,file=con)
  cat(listobject$whatsthis,append=T,fill=T,file=con)
  cat("",append=T,fill=T,file=con) # add space
  
  ### PLOT INDICATOR
  cat("## Key Results and Visualizations",append=T,fill=T,file=con)
  cat(listobject$visualizations,append=T,fill=T,file=con)
  cat("",append=T,fill=T,file=con) # add space
  
  ### rchunk code to run plot functions from ecodata
  # Check if plot function exists before executing
  if (exists(paste0("plot_",listobject$indicatorname))){
    # Generate code for all argument combinations as a list
    plot_code_strings <- ecodata::create_all_plots(ecodata_name = listobject$indicatorname, write_only = T, n = 10)

    # Iterate over each element in the 'plot_code_strings' list
    # to generate a string for each unique variation of the plot function
    for (i in 1:length(plot_code_strings)){
    # Extract argument values from 'plot_code_strings' to use in chunk name and plot header
    argument_value_strings <- regmatches(plot_code_strings[i], gregexpr("'[^']+'", plot_code_strings[i]))
      
    # Create code chunk name
    # Remove quotations and join 'argument_value_strings' into a single chunk name
    chunk_name <- paste0(gsub("'", "", unlist(argument_value_strings)), collapse = "")
      
    # Create plot header
    ## Remove quotations, join 'argument_value_strings' and separate with a comma
    plot_header <- paste0(gsub("'", "", unlist(argument_value_strings)), collapse = ",")
    ## Exclude MidAtlantic if MAB is present
    if (grepl("MAB", plot_header)){
      plot_header <- gsub("MidAtlantic", "", plot_header)
    }
    ## Exclude NewEngland if GB or GOM are present
    if (grepl("GB|GOM", plot_header)){
      plot_header <- gsub("NewEngland", "", plot_header)
    }
    ## Clean up plot header string
    ### Remove unused commas from plot header
    plot_header <- gsub("^,", "", plot_header) # Remove leading comma
    plot_header <- gsub(",$", "", plot_header) # Remove trailing comma
    plot_header <- gsub(",,", ",", plot_header) # Remove double comma
    ### Add space after all commas
    plot_header <- gsub(",", ", ", plot_header)
    ## Replace `ecodata` syntax with proper naming
    plot_header <- gsub("MAB", "Mid-Atlantic Bight", plot_header) # Explicitly write EPU name
    plot_header <- gsub("GB", "Georges Bank", plot_header) # Explicitly write EPU name
    plot_header <- gsub("GOM", "Gulf of Maine", plot_header) # Explicitly write EPU name
    plot_header <- gsub("MidAtlantic", "Mid-Atlantic", plot_header) # Explicitly write region name
    plot_header <- gsub("NewEngland", "New England", plot_header) # Explicitly write region name

    # Write plot function code chunk
    # Create plot header
    cat(paste0("**", plot_header, "**"),append=T,fill=T,file=con)
    # Open and name code chunk
    cat(paste0("```{r plot_", listobject$indicatorname, chunk_name, "}"),append=T,fill=T,file=con)
    # Write header to .Rmd  
    cat("# Plot indicator",append=T,fill=T,file=con)
    # Write plot code to .Rmd
    cat(paste0("ggplotObject <- ", plot_code_strings[i], " +"),append=T,fill=T,file=con)
    # Replace plot title with empty text
    cat(paste0("ggplot2::ggtitle('')"),append=T,fill=T,file=con)  
    # Print plot to .Rmd
    cat("ggplotObject",append=T,fill=T,file=con)
    # Create button to download plotted data
    ## Exclude pages with subtype spatial
    if (page_subtype != "spatial"){
      ## Call download_this function
      cat("try(downloadthis::download_this(",append=T,fill=T,file=con)
      ## Paste ecodata dataset name
      cat(paste0("ggplotObject$data,"),append=T,fill=T,file=con)
      ## Name output file using ecodata dataset name
      cat(paste0("output_name = '", listobject$indicatorname, chunk_name, "',"),append=T,fill=T,file=con)
      ## Set output extension to .csv
      cat("output_extension = '.csv',",append=T,fill=T,file=con)
      ## Define button label
      cat("button_label = 'Download the data plotted above',",append=T,fill=T,file=con)
      ## Define button type
      cat("button_type = 'default',",append=T,fill=T,file=con)
      ## Toggle on icon
      cat("has_icon = TRUE,",append=T,fill=T,file=con)
      ## Define which icon to use
      cat("icon = 'fa fa-save',",append=T,fill=T,file=con)
      ## Define button class
      cat("class = 'hvr-sweep-to-left',",append=T,fill=T,file=con)
      ## Set output mode to csv2
      cat("csv2 = F),",append=T,fill=T,file=con)
      ## Suppress warnings and errors
      cat("silent = TRUE)",append=T,fill=T,file=con)
    }
    # Close code chunk
    cat("```",append=T,fill=T,file=con)
    # Add break after the button
    cat("<br>",append=T,fill=T,file=con)
    # Add space after code chunk
    cat("",append=T,fill=T,file=con)
    }
  }
    
  ### IMPLICATIONS 
  cat("## Implications",append=T,fill=T,file=con)
  cat(listobject$implications,append=T,fill=T,file=con)
  cat("",append=T,fill=T,file=con) # add space

  ### SPATIAL + TEMPORAL SCALE
  cat("## Indicator statistics ",append=T,fill=T,file=con)
  cat(paste0("Spatial scale: ",listobject$indicatorStatsSpatial),append=T,fill=T,file=con)
  cat("",append=T,fill=T,file=con) # add space
  cat(paste0("Temporal scale: ",listobject$indicatorStatsTemporal),append=T,fill=T,file=con)
  cat("",append=T,fill=T,file=con) # add space
  
  ### VARIABLES FOUND IN DATA
  cat("**Variable definitions**",append=T,fill=T,file=con)
  cat("",append=T,fill=T,file=con) # add space

  # grabs the defined variables and writes out as an unordered list
  if (page_type == "independent") {
    cat(unlist(strsplit(listobject$defineVariables,"\\n")),append=T,fill=T,file=con)
    cat("",append=T,fill=T,file=con) # add space
  }

  # check to see if data name exists. Some do not but need a catalog page
  if (page_type == "ecodata") {  
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
  }
  
  cat("## Get the data",append=T,fill=T,file=con)
  cat("",append=T,fill=T,file=con) # add space
  
  ### CONTACT PERSON
  cat(paste0("**Point of contact**: ",listobject$poc),append=T,fill=T,file=con)
  cat("",append=T,fill=T,file=con) # add space
  
  if (page_type == "ecodata") {
    # List ecodata dataset name
    cat(paste0("**ecodata dataset**: `ecodata::",listobject$indicatorname,"`"),append=T,fill=T,file=con)
    cat("",append=T,fill=T,file=con) # add space
    # Provide tech doc link
    cat(paste0("**Tech Doc link**: <https://noaa-edab.github.io/tech-doc/",listobject$indicatorname,".html>"),append=T,fill=T,file=con)
    cat("",append=T,fill=T,file=con) # add space
  }
  
  ### PUBLIC AVAILABILITY + ACCESSIBILITY
  cat("### Public Availability",append=T,fill=T,file=con)
  cat("",append=T,fill=T,file=con) # add space
  cat(listobject$publicAvailability,append=T,fill=T,file=con)
  cat("",append=T,fill=T,file=con) # add space
  
  if (listobject$publicAvailability == "Source data are NOT publicly available.") {
    cat("### Accessibility Constraints",append=T,fill=T,file=con)
    cat("",append=T,fill=T,file=con) # add space
    cat(listobject$accessibility,append=T,fill=T,file=con)
    cat("",append=T,fill=T,file=con) # add space
  }

  # References are generated automatically  

  #close the connection
  close(con)

}