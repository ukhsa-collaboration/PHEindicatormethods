## Submission of package version 1.4.0

## Updates

* Package owner has a new contact email address due to organisational changes - contact details have been updated in DESCRIPTION file and this change can be confirmed via communication with the previous package contact email address which is still available and in use until September 2022.
* GitHub Actions Continuous Integration implemented on Git repo.
* Web URLs have been updated in documentation.
* Three new functions have been added to support funnel plot statistics.
* Two functions have had to be renamed due to ambiguous terminology - to maintain backwards compatibility, both the old- and new- named versions of these functions now co-exist in the package with the old-named functions badged as superceded using lifecycle.  The old-named functions will be deprecated in a future release. 
* functionality of the `phe_life_expectancy` function has been amended to include an additional suppression condition.


## R CMD check results

There were no ERRORs, WARNINGS or NOTES on local Windows 10 install using R 4.1.0 and RStudio 2022.02.2 Build 485


## Test Environments 

local windows 10 installs: R4.1.0 - OK   

Travis check on Linux (2020-06-18) - OK  

devtools::check_win gave no ERRORS, WARNINGS or NOTES on the following versions:  
* release:    R version 4.0.0 (2020-04-20)
* oldrelease: R version 3.6.3 (2020-02-29)

devtools::check_win gave no ERRORS OR WARNINGS but there was 1 NOTE about possible invalid DOIs which has been checked and is not critical to the functionality of the package:  
* devel:      R Under development (unstable) (2020-06-12 r78687)


* devtools::check_rhub has been run on the following platforms.  I had to use the argument **env_vars=c(R_COMPILE_AND_INSTALL_PACKAGES = "always")** as a workaround to avoid the error **Error in loadNamespace(name) : there is no package called 'utf8'** on Windows platform as described [here](https://github.com/r-hub/rhub/issues/374). These checks produced a NOTE about possible invalid DOIs which have been checked and are not critical to the functionality of the package. 
* Windows Server 2008 R2 SP1, R-devel, 32/64 bit 
* Ubuntu Linux 16.04 LTS, R-release, GCC
* Fedora Linux, R-devel, clang, gfortran  

## CRAN Checks for existing package release version v1.3.1
All checks have status OK.

## Downstream dependencies
There are no known downstream dependencies on CRAN
