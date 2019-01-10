context("test_phe_quantiles")

#library(dplyr)
#library(readxl)
#library(testthat)
## test line



#test calculations
test_that("quantiles calculate correctly",{
  expect_equal(phe_quantile(filter(test_quantiles_g,GroupSet == "IndSexReg"),Value, AreaCode, ParentCode,
                             invert = Polarity, inverttype = "field")[23],
               rename(filter(test_quantiles_g,GroupSet == "IndSexReg"),quantile = QuantileInGrp)[18],
                      check.attributes=FALSE, check.names=FALSE,info="test grouped df field")

  expect_equal(phe_quantile(filter(test_quantiles_g,IndSexRef == "90366Female" & GroupSet == "IndSexReg"),Value, AreaCode, ParentCode,
                             invert = FALSE)[24],
               rename(filter(test_quantiles_g,IndSexRef == "90366Female"& GroupSet == "IndSexReg"),quantile = QuantileInGrp)[18],
               check.attributes=FALSE, check.names=FALSE,info="test grouped df logical")

#  expect_equal(phe_quantile(filter(test_quantiles_g,GroupSet == "IndSex"), Value, AreaCode,
#                            nquantiles = 4L)[24],
#               rename(filter(test_quantiles_g,GroupSet == "IndSex"),quantile = QuantileInGrp)[18],
#               check.attributes=FALSE, check.names=FALSE,info="test grouped df logical nohighergeog")

  expect_equal(phe_quantile(test_quantiles_ug, Value, AreaCode, Ref_ug,
                             invert = Polarity, nquantiles = 7L, inverttype = "field")[24],
               rename(test_quantiles_ug,quantile = QuantileInGrp)[19],check.attributes=FALSE,
               check.names=FALSE,info="test ungrouped df field")

  expect_equal(phe_quantile(test_quantiles_ug, Value, AreaCode,
                             invert = FALSE, nquantiles = 4L)[25],
               rename(test_quantiles_ug,quantile = QuantileInGrp)[19],check.attributes=FALSE, check.names=FALSE,info="test ungrouped df logical")

  expect_equal(phe_quantile(test_quantiles_ug, Value, AreaCode,
                            invert = FALSE, nquantiles = 4L)[25],
               rename(test_quantiles_ug,quantile = QuantileInGrp)[19],check.attributes=FALSE, check.names=FALSE,info="test ungrouped df logical nohighergeog")

})


#test error handling
test_that("quantiles - errors are generated when invalid arguments are used",{
  expect_error(phe_quantile(test_quantiles_g),
               "function phe_quantile requires at least 3 arguments: data, values, basegeog",info="error invalid number of arguments")
  expect_error(phe_quantile(test_quantiles_g, Value, AreaCode, ParentCode,
                            invert = Polarity, inverttype = "vector"),
               "valid values for inverttype are logical and field",info="error inverttype is invalid")
  expect_error(phe_quantile(test_quantiles_g, Value, AreaCode, ParentCode,
                            invert = 6, inverttype = "logical"),
               "invert expressed as a logical must equal TRUE or FALSE",info="error logical invert is invalid")
  expect_error(phe_quantile(test_quantiles_g, AreaName, AreaCode, ParentCode,
                            invert = Polarity, inverttype = "field"),
               "values argument must be a numeric field from data",info="error value is invalid")
  expect_error(phe_quantile(test_quantiles_fail, Value, AreaCode, Ref_ug,
                            invert = Polarity, inverttype = "field"),
               "invert field values must take the same logical value for each data grouping set and highergeog",info="error invert varies")
})
