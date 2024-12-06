## CRAN submission of PHEindicatormethods package version 2.1.0

## Updates in this release

* Added `calculate_dsr` function as copy of `phe_dsr` function with additional functionality
* Soft-deprecated `phe_dsr` function (rename necessary to remove PHE branding of disbanded organisation).


## R CMD check results on local installation

There were no ERRORs, WARNINGS or NOTES on the following local installation:
* Windows 10 install using R 4.4.1


## Other Test Environments 

devtools::check_win_* on the following R versions gave no ERRORS OR WARNINGS.  
All release versions showed 1 NOTE about new maintainer because the forename 
and surname order has been corrected. The logs mention possible invalid URL but 
this has been checked menually the the package passes the urlchecker::url_check():  

* release:    R version 4.4.2 (2024-10-31 ucrt)  
* oldrelease: R version 4.3.3 (2024-02-29)  
* devel:      R under development (unstable) (2024-12-04 r87420 ucrt)  

R-hub-v2 checks have been performed on the Consortium Runners for the following 
platforms and all jobs completed successfully.
<https://github.com/r-hub2/some-psychogenic-africanporcupine-PHEindicatormethods/actions>  

* linux (any version) ubuntu-latest on GitHub  
* macOS (any version) macos-13 on GitHub  
* clang20 R-devel (2024-10-09 r87215) Ubuntu 22.04.5 LTS  
* ubuntu-release R-4.4.2 (2024-10-31) Ubuntu 22.04.5 LTS  


## CRAN Checks for existing package release

CRAN checks for version v2.0.2 are all OK


## Downstream dependencies

revdepcheck::revdep_check() shows no downstream dependencies
