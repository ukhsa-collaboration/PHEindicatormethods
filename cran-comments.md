## CRAN submission of PHEindicatormethods package version 1.4.1


## Updates

* Package maintainer has a new email address domain name due to organisational changes - contact details have been updated in DESCRIPTION file and maintainer still has access to the previous email to confirm the change until at least September 2022.
* GitHub Actions Continuous Integration implemented on Git repo.
* Expired web URLs have been updated in documentation.
* Three new functions have been added to support funnel plot statistics.
* Two functions have had to be renamed due to ambiguous terminology - to maintain backwards compatibility, both the old- and new- named versions of these functions now co-exist in the package with the old-named functions lifecycle badged and documented as deprecated and users signposted to the new alternatives.  The old-named functions will be deleted in a future release, not before December 2022.
* functionality of the `phe_life_expectancy` function has been amended to include an additional suppression condition.
* updated function documentation to use roxygen markdown

## R CMD check results on local installation

There were no ERRORs, WARNINGS or NOTES on the following local installation:
* Windows 10 install using R 4.1.0 and RStudio 2022.02.2 Build 485


## Other Test Environments 

devtools::check_win_* on the following R versions gave no ERRORS OR WARNINGS but there was 1 NOTE recognising the change to the package maintainer email address.  Additionally the devel check showed 3 possible invalid DOIs but stated that the service was unavailable to check them (These have been checked manually and also are not critical to functionality of package):  
* release:    R version 4.2.1 (2022-06-23 ucrt)  
* oldrelease: R version 4.1.3 (2022-03-10)  
* devel:      R Under development (unstable) (2022-08-02 r82660 ucrt)  

GitHub actions successfully ran R CMD Check on:  
* Windows-latest (release)
* ubuntu-latest (release)
* ubuntu-latest (devel)
* ubuntu-latest (oldrel-1) 

GitHub actions failed to run R CMD CHECK on MacOS-latest (release) - the failure was not always reproduced as continuous integration ran on dev and main branches.  It references an inability to install package MASS because it requires R >= 4.3 but this appears to be a known bug which is documented at the following sites:
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
