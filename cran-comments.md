## CRAN submission of PHEindicatormethods package version 1.4.2


## Updates

* Updated for compatibility with tidyselect v1.2.0 and purrr release due December 2022


## R CMD check results on local installation

There were no ERRORs, WARNINGS or NOTES on the following local installation:
* Windows 10 install using R 4.2.1 and RStudio 2022.07.1 Build 554


## Other Test Environments 

devtools::check_win_* on the following R versions gave no ERRORS OR WARNINGS.  There was 1 NOTE in R oldrelease about possibly misspelled words in the DESCRIPTION, but these spellings are all confirmed OK. There was 1 NOTE in R release about 3 possibly invalid DOIs but the note stated that the service was unavailable to check them (These have been checked manually and also are not critical to functionality of package). There were no NOTES in the devel release:  
* release:    R version 4.2.2 (2022-10-31) 
* oldrelease: R version 1.4.3 (2022-03-10)
* devel:      R Under development (unstable) (2022-11-28 r83388 ucrt)  

GitHub actions successfully ran R CMD Check on:  
* Windows-latest (release)
* ubuntu-latest (release)
* ubuntu-latest (devel)
* ubuntu-latest (oldrel-1)
* macos-latest (release) 


rhub::check() has been run on the following platforms with status OK.  
* Ubuntu Linux 20.04.1 LTS, R-release, GCC (ubuntu-gcc-release)
* macOS 10.13.6 High Sierra, R-release, brew (macos-highsierra-release)
* macOS 10.13.6 High Sierra, R-release, CRAN's setup (macos-highsierra-release-cran) 
* Debian Linux, R-devel, clang, ISO-8859-15 locale (debian-clang-devel)


## CRAN Checks for existing package release version v1.4.1

All checks have status OK.


## Downstream dependencies

revdepcheck::revdep_check() showed no downstream dependencies on CRAN
