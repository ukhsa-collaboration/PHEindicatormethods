
## Submission of updated package version 1.2.0
This package contains a bug fix for the phe_quantile function that results in backwards-incompatibility due to an argument being deprecated (will show as error if used for time being).  Also minor edits to utilise the rlang {{}} embrace notation, remove phe_sii warnings on tests and minor enhancements to functionality.




## R CMD check results
There were no ERRORs, WARNINGS or NOTES on local Windows 10 install using R 3.6.1 and RStudio 1.2.1335.

## Test Environments  
* local windows 10 installs: R3.6.1 - OK 

* Travis check on Linux (2019-09-03) - passed  

* devtools::check_win gave no ERRORS, WARNINGS or NOTES on the following versions: R version 3.5.3 (2019-03-11), R version 3.6.1 (2019-07-05) and R Under development (unstable) (2019-09-02 r77130).  

* devtools::check_rhub has been run on the following platforms: Debian Linux, R-devel, clang, ISO-8859-15 locale; Fedora Linux, R-devel, clang, gfortran; Windows Server 2008 R2 SP1, R-devel, 32/64 bit; Ubuntu Linux 16.04 LTS, R-release, GCC.  These build checks produced a couple of NOTES about possible mispellings, possible invalid DOIs which are all valid and not critical to the functioning of the package.

## Downstream dependencies
There are no known downstream dependencies
