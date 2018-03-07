
<!-- README.md is generated from README.Rmd. Please edit that file -->
PHEindicatormethods
===================

This is an R package to support analysts in the execution of statistical methods approved for use in the production of PHE indicators such as those presented via Fingertips. It provides functions for the generation of Proportions, Rates, DSRs, ISRs, SMRs and Means including confidence intervals for these statistics.

Any feedback would be appreciated and can be provided using the Issues section of the GitLab repository, or by emailing <PHDS@phe.gov.uk>

<br/> <br/>

Installation
------------

### From zip

Download this repository from GitLab and either build from source or do:

``` r
source <- devtools:::source_pkg("C:/path/to/PHEindicatormethods-master")
install(source)
```

### With devtools

You can install the latest version of PHEindicatormethods from GitLab with:

``` r
if (!require(devtools)) install.packages("devtools")
devtools::install_git('https://gitlab.phe.gov.uk/Georgina.Anderson/PHEindicatormethods', build_vignettes=TRUE)
```

<br/> <br/>

Contents
--------

The package contains the following functions and datasets - see individual item documentation for full details

**Functions:**
- byars\_lower
- byars\_upper
- phe\_dsr
- phe\_isr
- phe\_mean
- phe\_proportion
- phe\_rate
- phe\_smr
- wilson\_lower
- wilson\_upper

**Datasets:**
- esp2013
