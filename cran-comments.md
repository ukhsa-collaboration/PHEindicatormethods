## CRAN submission of PHEindicatormethods package version 1.4.0


## Updates

* Package maintainer has a new email address domain name due to organisational changes - contact details have been updated in DESCRIPTION file and maintainer still has access to the previous email to confirm the change until at least September 2022.
* GitHub Actions Continuous Integration implemented on Git repo.
* Expired web URLs have been updated in documentation.
* Three new functions have been added to support funnel plot statistics.
* Two functions have had to be renamed due to ambiguous terminology - to maintain backwards compatibility, both the old- and new- named versions of these functions now co-exist in the package with the old-named functions badged and documented as superceded.  The old-named functions will be deprecated in a future release. 
* functionality of the `phe_life_expectancy` function has been amended to include an additional suppression condition.


## R CMD check results on local installation

There were no ERRORs, WARNINGS or NOTES on the following local installation:
* Windows 10 install using R 4.1.0 and RStudio 2022.02.2 Build 485


## Other Test Environments 

devtools::check_win_* on the following R versions gave no ERRORS OR WARNINGS but there was 1 NOTE about a possible invalid DOI - this URL has been checked and is still valid (also not critical to functionality of the package).  The new maintainer email address is also cited in these logs:  
* release:    R version 4.2.1 (2022-06-23 ucrt)  
* oldrelease: R version 4.1.3 (2022-03-10)  
* devel:      R Under development (unstable) (2022-07-31 r82648 ucrt)  

GitHub actions successfully ran R CMD Check on:  
* Windows-latest (release)
* ubuntu-latest (release)
* ubuntu-latest (devel)
* ubuntu-latest (oldrel-1)
* MacOS-latest (release) 

When GitHub actions ran R CMD CHECK on the dev branch before merging to master it failed on MacOS-latest (release) - the failure references an inability to install package MASS because it requires R >= 4.3 but this appears to be a known bug which is documented at the following sites and this result was not reproducible:
* https://github.com/r-lib/pak/issues/393  
* https://community.rstudio.com/t/github-actions-failing-for-macos-cannot-install-packages/139574

rhub::check() has been run on the following platforms with status OK.  
* Ubuntu Linux 20.04.1 LTS, R-release, GCC
* macOS 10.13.6 High Sierra, R-release, brew
* macOS 10.13.6 High Sierra, R-release, CRAN's setup
* Debian Linux, R-devel, clang, ISO-8859-15 locale


## CRAN Checks for existing package release version v1.3.2

All checks have status OK.


## Downstream dependencies

revdepcheck::revdep_check() showed no downstream dependencies on CRAN
