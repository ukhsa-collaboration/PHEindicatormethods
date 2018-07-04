
<!-- README.md is generated from README.Rmd. Please edit that file -->
PHEindicatormethods
===================

This is an R package to support analysts in the execution of statistical methods approved for use in the production of PHE indicators such as those presented via Fingertips. It provides functions for the generation of Proportions, Rates, DSRs, ISRs, SMRs and Means including confidence intervals for these statistics.

Any feedback would be appreciated and can be provided using the Issues section of the GitHub repository, or by emailing <PHDS@phe.gov.uk>

<br/> <br/>

Installation
------------

#### From GitHub using devtools

You can install the latest version of PHEindicatormethods from GitHub with:

``` r
if (!require(devtools)) install.packages("devtools")

devtools::install_github("PublicHealthEngland/PHEindicatormethods",
                         build_vignettes = TRUE,
                         dependencies = "suggests")
```

Some users may receive a Lazy Load error following installation and loading of this package using devtools in RStudio. In these cases, please close and re-open RStudio and then re-load the package using the library command.

#### From zip

Download this repository from GitHub and either build from source or do:

``` r
source <- devtools:::source_pkg("C:/path/to/PHEindicatormethods-master")
devtools::install(source)
```

<br/> <br/>

Package Versioning
------------------

Following installation of this package, type 'packageVersion("PHEindicatormethods")' in the R console to show the package version. If it is suffixed with a 9000 number then you are using an unapproved development version.

Released versions of this package will have version numbers consisting of three parts: <major>.<minor>.<patch> In-development versions of this package will have a fourth component, the development version number, which will increment from 9000.

See <http://r-pkgs.had.co.nz/description.html> for further information on package versioning

Package Contents
----------------

The package contains the following functions, datasets and vignettes - see individual item documentation for full details

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

**Vignettes:**
- Vignette for calculating DSRs for multiple geographies and time periods
(type 'browseVignettes("PHEindicatormethods")' to view in HTML)
