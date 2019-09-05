
## Submission of updated package version 1.1.5
This package release includes edits to phe_sii.R to resolve issues with upstream dependencies on tidyr version 1.0.0 nest and unnest functions due for release 9th Septmber 2019. This version 1.1.5 also still works with current tidyr version 0.8.3

## R CMD check results
There were no ERRORs, WARNINGS or NOTES on local Windows 10 install using R 3.6.1 and RStudio 1.2.1335.

## Test Environments  
* local windows 10 installs: R3.6.1 - OK 

* Travis check on Linux (2019-09-03) - passed  

* devtools::check_win gave no ERRORS, WARNINGS or NOTES on the following versions: R version 3.5.3 (2019-03-11), R version 3.6.1 (2019-07-05) and R Under development (unstable) (2019-09-02 r77130).  

* devtools::check_rhub has been run on the following platforms: Debian Linux, R-devel, clang, ISO-8859-15 locale; Fedora Linux, R-devel, clang, gfortran; Windows Server 2008 R2 SP1, R-devel, 32/64 bit; Ubuntu Linux 16.04 LTS, R-release, GCC.  These build checks produced a couple of NOTES about possible mispellings, possible invalid DOIs which are all valid and not critical to the functioning of the package.

## Downstream dependencies
There are no known downstream dependencies
