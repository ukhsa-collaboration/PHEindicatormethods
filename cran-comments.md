
## INCOMPLETE


## Submission of package version 1.2.1
This package contains enhancements to the phe_sii and phe_life_expectancy functions.
An output variable to phe_dsr() that was added in version 1.2.0 has been removed as it was calculating a nonsensical value.

## R CMD check results
There were no ERRORs, WARNINGS or NOTES on local Windows 10 install using R 3.6.3 and RStudio 1.2.5019

## Test Environments  
* local windows 10 installs: R3.6.3 - OK 

* Travis check on Linux (2020-01-13) - passed  

* devtools::check_win gave no ERRORS, WARNINGS or NOTES on the following versions: R version 3.5.3 (2019-03-11), R version 3.6.3 (2019-12-12) and R Under development (unstable) (2020-01-28 r77738).  

* devtools::check_rhub has been run on the following platforms: Fedora Linux, R-devel, clang, gfortran; Windows Server 2008 R2 SP1, R-devel, 32/64 bit; Ubuntu Linux 16.04 LTS, R-release, GCC.  These build checks produced a couple of NOTES about possible mispellings, possible invalid DOIs which are all valid and not critical to the functioning of the package.

## Downstream dependencies
There are no known downstream dependencies on CRAN
