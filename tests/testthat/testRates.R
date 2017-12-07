library(testthat)
library(readxl)

context("test_phe_rates")

# import test data
testdata_Rate_100000 <- read_excel(".\\tests\\testthat\\testdata_Rate.xlsx",
                              sheet="testdata_Rate_100000", col_names=TRUE)
testdata_Rate_100 <- read_excel(".\\tests\\testthat\\testdata_Rate.xlsx",
                                sheet="testdata_Rate_100", col_names=TRUE)

#test calculations
test_that("rates and CIs calculate correctly",{
  expect_equal(phe_rate(testdata_Rate_100$Numerator,testdata_Rate_100$Denominator,multiplier=100)[2:6],
               select(testdata_Rate_100,c(1,2,4,5,6)),check.attributes=FALSE, check.names=FALSE)
  expect_equal(phe_rate(testdata_Rate_100000$Numerator,testdata_Rate_100000$Denominator,
                        multiplier=100000)[2:6],
               select(testdata_Rate_100000,c(1,2,4,5,6)),check.attributes=FALSE, check.names=FALSE)
  expect_equal(phe_rate(testdata_Rate_100$Numerator,testdata_Rate_100$Denominator,
                        conf.level=99.8, multiplier=100)[2:6],
               select(testdata_Rate_100,c(1,2,4,7,8)),check.attributes=FALSE, check.names=FALSE)
  expect_equal(phe_rate(testdata_Rate_100000$Numerator,testdata_Rate_100000$Denominator,
                        conf.level=99.8, multiplier=100000)[2:6],
               select(testdata_Rate_100000,c(1,2,4,7,8)),check.attributes=FALSE, check.names=FALSE)
})


# test error handling
test_that("errors are generated when invalid arguments are used",{
  expect_error(phe_rate(-65,100),"numerators must be greater than or equal to zero")
  expect_error(phe_rate(65,-100),"denominators must be greater than zero")
  expect_error(phe_rate(65,0),"denominators must be greater than zero")
  expect_error(phe_rate(65,100,conf.level=20),"confidence level must be >= 90 and <= 100 (or >= 0.9 and <= 1)")
})
