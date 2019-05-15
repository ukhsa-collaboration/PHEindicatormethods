
## Resubmission of updated package version 1.1.3
This package release v1.1.3 will replace the previous release v1.1.2 which was removed from CRAN and archived on 2019-05-12 due to an issue with a test script on platforms using clang as compiler reported by CRAN check_results.

The issue became apparent following release of v1.1.1 on 18/04/2019, at which time I received notification that 
some tests were failing with non-sensical error messages on r-devel-linux-x86_64-fedora-clang and
r-devel-linux-x86_64-debian-clang and r-oldrel-windows-ix86+x86_64. I have since amended and resubmitted (v1.1.2) but the issue persisted. 

In this resubmission I have re-coded the affected test in testProportions.R to resolve the issue although I have been unable to retest on the affected operating system environments.  The package has been tested using devtools::rhub_check() which produces only a couple of NOTES about possible misspellings, possible invalid URLs and possible invalid DOIS on some platforms but these are all valid and also not critical to the functioning of the package.  The non-sensical error previously decribed was not produced.

There are minor changes to the function documentation.

## Test Environments
* local windows 10 install: R 3.5.3 - OK
* Travis check on Linux (2019-05-14) - passed
* devtools::check_win gave 1 NOTE (devel R 3.6.0 beta, release R 3.5.3 and oldrelease R 3.4.4).  The note related to either possible misspellings or possible invalid urls containing spaces but both are OK.
* rhub_check gave a couple of NOTES about possible invlid spellings, possible invalid URLS and possible invalid DOIs which are all OK.

## R CMD check results
There were no ERRORs, WARNINGS or NOTES.

## Downstream dependencies
There are no known downstream dependencies
