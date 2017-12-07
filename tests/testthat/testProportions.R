library(testthat)
library(readxl)

context("test_phe_proportions")

# import test data
testdata_Prop_1 <- read_excel(".\\tests\\testthat\\testdata_Proportion.xlsx",
                              sheet="testdata_Prop_1", col_names=TRUE)
testdata_Prop_100 <- read_excel(".\\tests\\testthat\\testdata_Proportion.xlsx",
                                sheet="testdata_Prop_100", col_names=TRUE)

# test calculations
test_that("proportions and CIs calculate correctly",{
  expect_equal(phe_proportion(testdata_Prop_100$Numerator,testdata_Prop_100$Denominator,
                              percentage=TRUE)[2:6],
               select(testdata_Prop_100,c(1,2,4,5,6)),check.attributes=FALSE, check.names=FALSE)
  expect_equal(phe_proportion(testdata_Prop_1$Numerator,testdata_Prop_1$Denominator,
                              percentage=FALSE)[2:6],
               select(testdata_Prop_1,c(1,2,4,5,6)),check.attributes=FALSE, check.names=FALSE)
  expect_equal(phe_proportion(testdata_Prop_100$Numerator,testdata_Prop_100$Denominator,
                              conf.level=99.8, percentage=TRUE)[2:6],
               select(testdata_Prop_100,c(1,2,4,7,8)),check.attributes=FALSE, check.names=FALSE)
  expect_equal(phe_proportion(testdata_Prop_1$Numerator,testdata_Prop_1$Denominator,
                              conf.level=99.8, percentage=FALSE)[2:6],
               select(testdata_Prop_1,c(1,2,4,7,8)),check.attributes=FALSE, check.names=FALSE)
})


# test error handling
test_that("errors are generated when invalid arguments are used",{
  expect_error(phe_proportion(-65,100),"numerators must be greater than or equal to zero")
  expect_error(phe_proportion(65,-100),"denominators must be greater than zero")
  expect_error(phe_proportion(65,0),"denominators must be greater than zero")
  expect_error(phe_proportion(100,65),"numerators must be less than or equal to denominator for a proportion statistic")
  expect_error(phe_proportion(65,100,conf.level=20),"confidence level must be >= 90 and <= 100 (or >= 0.9 and <= 1)")
  expect_error(phe_proportion(c(5,65,90,98),c(100,100)),
               "numerator and denominator vectors must be of equal length")
})

