## Submission of package version 1.3.1
Functions have been updated to use the new .groups argument in dplyr::summarise to ensure grouping sets remain unchanged on output objects

## R CMD check results
There were no ERRORs, WARNINGS or NOTES on local Windows 10 install using R 4.0.0 and RStudio 1.2.5042

## Test Environments  
local windows 10 installs: R4.0.0 - OK   

Travis check on Linux (2020-04-11) - OK  

devtools::check_win gave no ERRORS, WARNINGS or NOTES on the following versions:  
* R version 4.0.0 alpha (2020-03-26 r78078)
* R version 3.6.3 (2020-02-29)
* R version 3.5.3 (2019-03-11)

* devtools::check_rhub has been run on the following platforms.  These build checks produced a couple of NOTES about possible mispellings, possible invalid DOIs which are all valid and not critical to the functioning of the package:  
* Fedora Linux, R-devel, clang, gfortran  
* Windows Server 2008 R2 SP1, R-devel, 32/64 bit 
* Ubuntu Linux 16.04 LTS, R-release, GCC


## CRAN Checks for current package release version v1.3.0
There is currently a WARNING against r-patched-osx-x86_64 which relates to an issue loading the package vignette.  A message on the r-package-devel distribution list on 12-04-2020 suggests this is currently happening for all packages that use vignettes.  I am unable to resolve this without access to this OS. The vignettes seem to work Ok elsewhere and are not essential to the package functionality.  I have added a comment to the README file offering alternative signposting to package vignettes on CRAN package webpage.

## Downstream dependencies
There are no known downstream dependencies on CRAN
