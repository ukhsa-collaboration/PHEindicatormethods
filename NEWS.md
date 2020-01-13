## PHEindicatormethods v1.2.0
The following changes may affect backwards compatibility with earlier versions of the package:  

* phe_quantile - highergeog argument deprecated.  Argument wasn't providing any additional functionality and its use was affecting grouping sets on output data frame.  Input dataframe must now be pre-grouped to reproduce the results previously obtained using the highergeog argument.  Also documentation references to geographies removed as function is applicable to other subgroups.

<br>

The following changes will not affect backwards compatibility with earlier versions of the package: 

* Amended functions to use new embrace interpolation features of rlang 0.4.0: dsr, isr, smr, rate, proportion, mean, quantile.  Package dependencies now require Rlang version 0.4.0 or higher   

* Added ability to output both 95% and 99.8% confidence intervals in a single execution to dsr, isr, smr, rate, proportion, mean functions

* Amended phe_dsr to also output the standardised population used in the calculation when type="full"

* Some warnings that occurred with `phe_life_expectancy()` have been fixed

## PHEindicatormethods v1.1.5
phe_sii function updated to be compatible with nest and unnest functions from tidyr version 1.0

## PHEindicatormethods v1.1.4
No significant changes.
Removed confusing line of commented out code from phe_life_expectancy calculation
Amended vignette title

## PHEindicatormethods v1.1.3

Minor amendments to package testing scripts to work on platforms using clang as compiler.  
Updates will not affect end users using other platforms.  

## PHEindicatormethods v1.1.2

Minor amendments to package testing scripts to work on platforms using clang as compiler.
Updates will not affect end users using other platforms.  


## PHEindicatormethods v1.1.1

The following changes may affect backwards compatibility with earlier versions of the package:  

* phe_proportion: Replaced logical PERCENTAGE argument with a numeric MULTIPLIER argument to enable proportions to be expressed more flexibly (eg per 1000). Default is now multiplier = 1 which gives equivalent output to the previous default of percentage = FALSE.  There is loss of backwards compatibility where the percentage argument has previously been specified in the function call - please replace 'percentage=TRUE' with 'multiplier=100' and 'percentage=FALSE' with 'multiplier=1'.

* all functions using the TYPE argument: Altered the default value for the TYPE argument from 'standard' to 'full'.  This may affect backwards compatibility with earlier versions of the package - where type has not been specified output will now contain additional metadata columns - to remove these please specify type = "standard" as a function argument.

* `byars_lower()`, `byars_upper()`, `wilson_lower()` and `wilson_upper()` functions have been moved to the package utilities and are less readily available to be used as stand-alone functions within the package.  These functions are intended to be called using other 'phe-' prefixed package functions.  

<br>

The following changes will not affect backwards compatibility with earlier versions of the package:  

* `phe_life_expectancy()` function added for calculation of life expectancy at different ages based on population and death data

* `phe_quantile()` function added for assigning data to quantiles

* `phe_sii()` function added for calculation of slope index of inequality and relative index of inequality

* `phe_proportion()` and `phe_rate()` functions now return aggregate data when grouped data is passed

* Altered handling of NA values in `phe_proportion()`, `phe_rate()` and `phe_mean()` functions to enable results to be returned for the records which do not contain invalid NAs 

* Altered handling of NA values in `phe_dsr()` function such that NA values in the age-specific count data are assumed to be equal to zero and no longer cause the function to error

* Altered handling of NA values in `phe_isr()` and `phe_smr()` functions such that NA values in the age-specific count, age-specific population and/or reference count data are assumed to be equal to zero and no longer cause the functions to error

 


## PHEindicatormethods V1.0.8
This is the first release of this package to CRAN
