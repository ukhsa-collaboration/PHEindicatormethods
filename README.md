
<!-- README.md is generated from README.Rmd. Please edit that file -->

[![test-coverage](https://github.com/ukhsa-collaboration/PHEindicatormethods/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/ukhsa-collaboration/PHEindicatormethods/actions/workflows/test-coverage.yaml)
[![R-CMD-check](https://github.com/ukhsa-collaboration/PHEindicatormethods/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ukhsa-collaboration/PHEindicatormethods/actions/workflows/R-CMD-check.yaml)

# PHEindicatormethods

This is an R package to support analysts in the execution of statistical
methods approved for use in the production of PHE indicators such as
those presented via Fingertips. It provides functions for the generation
of Proportions, Rates, DSRs, ISRs, Means, Life Expectancy and Slope
Index of Inequality including confidence intervals for these statistics,
and a function for assigning data to quantiles.

Any feedback would be appreciated and can be provided using the Issues
section of the [PHEindicatormethods GitHub
repository](https://github.com/ukhsa-collaboration/PHEindicatormethods).

<br/> <br/>

## Installation

#### Install from CRAN

Install the latest release version of PHEindicatormethods directly from
CRAN with:

``` r
install.packages("PHEindicatormethods")
```

<br/> <br/>

#### Install a development version from GitHub using remotes package

You can install a development version of PHEindicatormethods from GitHub
with:

``` r
if (!require(remotes)) install.packages("remotes")

remotes::install_github("ukhsa-collaboration/PHEindicatormethods",
                         build_vignettes = TRUE,
                         dependencies = TRUE,
                         build_opts = c("--no-resave-data"))
```

<br/> <br/>

## Package Versioning

Following installation of this package, type
‘packageVersion(“PHEindicatormethods”)’ in the R console to show the
package version. If it is suffixed with a 9000 number then you are using
an unapproved development version.

Released versions of this package will have version numbers consisting
of three parts:

major.minor.patch

In-development versions of this package will have a fourth component,
the development version number, which will increment from 9000.

See <https://r-pkgs.org/lifecycle.html> for further information on
package versioning
