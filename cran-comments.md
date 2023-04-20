## CRAN submission of PHEindicatormethods package version 2.0.1

## Updates in this release

* Updated ISRate and ISRatio functions to offer greater flexibility of inputs
* amended all GitHub url references for bug reporting and GitHub Action badges as 
code hosting transferred from PublicHealthEngland to UKHSA


## R CMD check results on local installation

There were no ERRORs, WARNINGS or NOTES on the following local installation:
* Windows 10 install using R 4.2.1 and RStudio 2022.07.2 Build 576


## Other Test Environments 

devtools::check_win_* on the following R versions gave no ERRORS OR WARNINGS.  
All 3 versions showed 1 NOTE about possibly misspelled words in the DESCRIPTION 
or possible invalid DOIs, but these have all been checked manually and confirmed 
OK:  
* release:    R version 4.2.2 (2022-10-31 ucrt) 
* oldrelease: R version 4.1.3 (2022-03-10) 
* devel:      R Under development (unstable) (2023-03-02 r83926 ucrt)


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


## CRAN Checks for existing package release version v1.4.2

The package was removed from CRAN on 01/03/2023 due to test failures caused by 
updates to dplyr in v 1.1.0 (fixed in this release).


## Downstream dependencies

revdepcheck::revdep_check() has previously shown no downstream dependencies on 
CRAN. Unable to run currently due to package being archived on CRAN - 
re-instating the package can only be helpful if there are any new downstream 
dependencies since the previous release.
