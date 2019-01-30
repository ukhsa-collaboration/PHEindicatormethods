## PHEindicatormethods v1.0.9

* Replaced logical PERCENTAGE argument in phe_proportion function with a numeric MULTIPLIER argument to enable proportions to be expressed more flexibly (eg per 1000). Default remains as multiplier = 1 equivalent to previous default of percentage=FALSE but there is loss of backwards compatibility where the percentage argument has previously been specified as TRUE in the function call - please replace 'percentage=TRUE' with 'multiplier=100'.   
* Added phe_quantile function
* Enabled phe_proportion and phe_rate functions to return aggregate data when grouped data is passed
* Altered handling of NA values in byars_lower, byars_upper, wilson_lower, wilson_upper, phe_proportion, phe_rate and phe_mean functions to enable results to be returned for records with valid inputs
* Altered handling of NA values in phe_dsr function such that NA values in the age-specific count data are assumed to be equal to zero
*  Altered handling of NA values in phe_isr and phe_smr functions such that NA values in the age-specific count, age-specific population and/or reference count data are assumed to be equal to zero


## PHEindicatormethods V1.0.8
This is the first release of this package to CRAN
