
## Resubmission
I have amended the description text in the DESCRIPTION file to include doi 
<doi:10.prefix.suffix> or 
ISBN references to the statistical methods used by the package functions. 
I also made the description text more concise.

I also updated one of the references cited in the function documentation for 
the phe_rate, phe_isr and phe_smr functions.

This package is new to CRAN.

## Description
This is an R package to support analysts in the execution of statistical methods approved for
use in the production of PHE indicators such as those presented via Fingertips. It provides
functions for the generation of Proportions, Rates, DSRs, ISRs, SMRs and Means including
confidence intervals for these statistics.

## Test Environments
* local Windows 7 install, R 3.5.1
* local windows 10 install, R 3.5.1
* used Travis to check on Linux (2018-7-11)
* devtools::build_win() generated 1 note (2018-07-06)

## R CMD check results
There were no ERRORs or WARNINGs.
R CMD check generated one note (because this is a new package).

## Downstream dependencies
This is a new package so there are no downstream dependencies
