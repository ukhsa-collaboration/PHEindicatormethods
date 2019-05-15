
## Resubmission of updated package version 1.1.3
This package release v1.1.3 will replace the previous release v1.1.2 which was removed from CRAN and archived on 2019-05-12 due to an issue with a test script on platforms using clang as compiler reported by CRAN check_results.

The issue became apparent following release of v1.1.1 on 18/04/2019, at which time I received notification that 
some tests were failing with non-sensical error messages on r-devel-linux-x86_64-fedora-clang and
r-devel-linux-x86_64-debian-clang and r-oldrel-windows-ix86+x86_64. I have since amended and resubmitted (v1.1.2) but the issue persisted. 

In this resubmission I have re-coded the affected test in testProportions.R to resolve the issue although I have been unable to retest on the affected operating system environments.  

There are minor changes to the function documentation.

## Test Environments  
* local windows 10 installs: R 3.5.3 and R3.6.0 - both OK 

* Travis check on Linux (2019-05-14) - passed  

* devtools::check_win gave 1 NOTE on all versions (R version 3.5.3 (2019-03-11), R version 3.6.0 (2019-04-26) and R Under development (unstable) (2019-05-14 r76503)).  The note related to either possible misspellings or possible invalid urls containing spaces but both are OK. 

* devtools::check_rhub has been run on the following platforms: Debian Linux, R-devel, clang, ISO-8859-15 locale; Fedora Linux, R-devel, clang, gfortran; Ubuntu Linux 16.04 LTS, R-release, GCC; Windows Server 2008 R2 SP1, R-devel, 32/64 bit; Windows Server 2008 R2 SP1, R-release, 32/64 bit.  These build checks produced a couple of NOTES about possible invalid spellings, possible invalid URLS and possible invalid DOIs which are all valid and not critical to the functioning of the package.  The non-sensical error message mentioned earlier (and the cause for failing CRAN checks previously) was not reported.

## R CMD check results
There were no ERRORs, WARNINGS or NOTES.

## Downstream dependencies
There are no known downstream dependencies
