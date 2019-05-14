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
