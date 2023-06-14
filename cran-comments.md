## CRAN submission of PHEindicatormethods package version 2.0.1

## Updates in this release

* Updated `calculate_ISRate` and `calculate_ISRatio` functions to offer greater flexibility of inputs
* amended all GitHub url references for bug reporting and GitHub Action badges as code hosting transferred from PublicHealthEngland to UKHSA-collaboration


## R CMD check results on local installation

There were no ERRORs, WARNINGS or NOTES on the following local installation:
* Windows 10 install using R 4.2.1


## Other Test Environments 

devtools::check_win_* on the following R versions gave no ERRORS OR WARNINGS.  
All release version showed 1 NOTE about possible invalid DOIs and oldrelease 
showed 1 NOTE about possibly misspelled words in the DESCRIPTION, but these 
have all been checked manually and confirmed as correct:  
* release:    R version 4.2.3 (2023-03-15 ucrt) 
* oldrelease: R version 4.1.3 (2022-03-10) 
* devel:      R version 4.3.0 RC (2023-04-18 r84287 ucrt)


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


## CRAN Checks for existing package release

CRAN checks for version v2.0.0 are all OK


## Downstream dependencies

revdepcheck::revdep_check() shows no downstream dependencies
