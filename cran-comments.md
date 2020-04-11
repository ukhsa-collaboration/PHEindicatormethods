## Submission of package version 1.3.1
Five functions within the package have been amended to ensure continued compatability with dplyr v1.0.0 due for release 1st May 2020.

Additionally, the phe_life_expectancy function has been amended to show more relevant population and death counts in the output.

## R CMD check results
There were no ERRORs, WARNINGS or NOTES on local Windows 10 install using R 3.6.3 and RStudio 1.2.5019

## Test Environments  
* local windows 10 installs: R3.6.3 - OK 

* Travis check on Linux (2020-04-11) - passed  

* devtools::check_win gave no ERRORS, WARNINGS or NOTES on the following versions: R version 3.5.3 (2019-03-11), R version 3.6.3 (2020-02-29) and R Under development (unstable) (2020-01-28 r77738).  

* devtools::check_rhub has been run on the following platforms: Fedora Linux, R-devel, clang, gfortran; Windows Server 2008 R2 SP1, R-devel, 32/64 bit; Ubuntu Linux 16.04 LTS, R-release, GCC.  These build checks produced a couple of NOTES about possible mispellings, possible invalid DOIs which are all valid and not critical to the functioning of the package.

## Downstream dependencies
There are no known downstream dependencies on CRAN
