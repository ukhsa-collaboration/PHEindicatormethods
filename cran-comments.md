
## First submission of updated package version 1.1.1
This package release v1.1.1 replaces the previous release v1.0.8 currently available on CRAN

## Amendments in this version
3 new functions have been added:  

* phe_quantiles
* phe_life_expectancy
* phe_sii

Also, minor amendments to existing functions.  These may result in backwards incompatibility - the circumstances when this may occur are clearly stated in the NEWS.md file and in the function documentation with solutions for how to make old code compatible with the later package version (eg logical 'percentage' argument replaced with numeric 'multiplier' argument - percentage = TRUE equivalent to multiplier = 100 etc).

## Test Environments
* local windows 10 install: R 3.5.1 and R 3.5.3 - Both OK
* Travis check on Linux (2019-04-18) - passed
* devtools::check_win gave 1 NOTE (devel R 3.6.0 beta, release R 3.5.3 and oldrelease R 3.4.4).  The note related to either possible misspellings or possible invalid urls containing spaces but both are OK.

## R CMD check results
There were no ERRORs, WARNINGS or NOTES.

## Downstream dependencies
There are no known downstream dependencies
