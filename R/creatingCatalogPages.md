## How to create a catalog entry

**NOTE: Running the function `build_rmd_from_issue.r` with argument `pullAllIssues = T` will completely rebuild the catalog. Any custom edits to the rmds will be lost. This should be avoided**

The default is `pullAllIssues=F`

#### Create the Catalog from scratch

* One finalized this should NEVER be repeated or content will be overwritten

    > build_rmd_from_issue(pullAllIssues = T)


#### Create rmd for a New submission (single issue)

* When a new submission has been completed and it needs to be added to catalog

* Determine the [issue number](https://github.com/NOAA-EDAB/catalog/issues) of the submission.
* Each data submission issue will have a green "submission" label associated with it. Underneath this label there will be a number. This is the issue number

* Run the function

    > build_rmd_from_issue(pullSingleIssue = T, issueNum = x)

where `x` is the number of the issue.


#### Create rmd for a submission already pulled


* When a you need to make changes

* Determine the [issue number](https://github.com/NOAA-EDAB/catalog/issues) of the submission.
* Each data submission issue will have a green "submission" label associated with it. Underneath this label there will be a number. This is the issue number

* Run the function

    > build_rmd_from_issue(issueNum = x)

#### Add entry to _bookdown.yml

* Add the name of the rmd to the `_bookdown.yml`.  This can be found in the root directory


#### Internal workings

`build_rmd_from_issue` will either pull all issues from GitHub (`pull_all_issues.r`), pull a single issue from GitHub (`pull_single_issue.r`) or use a list of issues that have been saved to `data-raw` from an earlier pull (This latter option is useful for debugging since GitHub will only allow so many API requests per hour)

* The pulled issues then get processed
  * The issue gets parsed into useful components (`parse_issue.r`)
  * The parsed issue is then turned into a list with readable names (`create_listobject.r`)
  * The list is then used to create the rmd (`make_rmd.r`). This is the file that handles the template layout of the resulting rmd.
  
* A rmd will be created in the `chapters` folder. Its name will be the name of the indicator. This name can be found in the issue under the heading `Indicator Name (as exists in ecodata)`

* For some indicators, the resulting rmd may need to be hand edited for indicator specific information. In particular, references. NOTE: If the rmd is created again from its issue then this hand edited content will be lost.

