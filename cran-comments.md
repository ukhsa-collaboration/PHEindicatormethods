
## Resubmission of updated package version 1.1.2
Following recent release of package v1.1.1 to CRAN on 18/04/2019, I then received notification that 
some tests were failing with nonsensical error messages on r-devel-linux-x86_64-fedora-clang and
r-devel-linux-x86_64-debian-clang and r-oldrel-windows-ix86+x86_64. See <https://cran.r-project.org/web/checks/check_results_PHEindicatormethods.html>

In this resubmission I have re-coded the affected tests in testProportions.R and testLifeExpectancy.R to resolve these issues although I have been unable to retest on the affected operating system environemnts.

This package release v1.1.2 will replace the previous release v1.1.1 currently available on CRAN

No changes have been made to the function code or documentation but I have incremented the version number and news file.

## Test Environments
* local windows 10 install: R 3.5.1 and R 3.5.3 - Both OK
* Travis check on Linux (2019-04-18) - passed
* devtools::check_win gave 1 NOTE (devel R 3.6.0 beta, release R 3.5.3 and oldrelease R 3.4.4).  The note related to either possible misspellings or possible invalid urls containing spaces but both are OK.

## R CMD check results
There were no ERRORs, WARNINGS or NOTES.

## Downstream dependencies
There are no known downstream dependencies
