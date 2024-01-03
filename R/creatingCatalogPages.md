## How to create a catalog entry

**NOTE: Running the function `build_rmd_from_issue.r` with argument `issuenum = NULL` will completely rebuild the catalog. Any custom edits to the rmds will be lost. This should be avoided**

#### Create rmd for a single submission

* Determine the [issue number](https://github.com/NOAA-EDAB/catalog/issues) of the submission.
* Each data submission issue will have a green "submission" label associated with it. Underneath this label there will be a number. This is the issue number

* Run the function `build_rmd_from_issue.r` with argument `issuenum = x` where `x` is the number of the issue. For example, If the issue number for the new submission is #99.

    > build_rmd_from_issue(issuenum = 99)

* This function calls on `pasrse_single_issue.r` and `make_rmd`. The `make_rmd` function handles the template layout of the resulting rmd.
    
* A rmd will be created in the `chapters` folder. Its name will be the name of the indicator. This name can be found in the issue under the heading `Indicator Name (as exists in ecodata)`

* For some indicators, the resulting rmd may need to be hand edited for indicator specific information. In particular, to produce alternative plots. NOTE: If the rmd is created again from its issue then this hand edited content will be lost.

* Add the name of the rmd to the `_bookdown.yml`.  This can be found in the root directory