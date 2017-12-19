library(testthat)
library(readxl)

context("test_phe_proportion")


# test calculations
test_that("proportions and CIs calculate correctly",{
  expect_equal(phe_proportion(test_Prop_100$Numerator, test_Prop_100$Denominator,
                              test_Prop_100$Area, percentage=TRUE)[1:6],
               select(test_Prop_100,c(1,2,3,5,6,7)),check.attributes=FALSE, check.names=FALSE, info="test 1")
  expect_equal(phe_proportion(test_Prop_1$Numerator, test_Prop_1$Denominator,
                              test_Prop_100$Area, percentage=FALSE)[1:6],
               select(test_Prop_1,c(1,2,3,5,6,7)),check.attributes=FALSE, check.names=FALSE, info="test 2")
  expect_equal(phe_proportion(test_Prop_100$Numerator,test_Prop_100$Denominator,
                              test_Prop_100$Area, conf.level=99.8, percentage=TRUE)[1:6],
               select(test_Prop_100,c(1,2,3,5,8,9)),check.attributes=FALSE, check.names=FALSE, info="test 3")
  expect_equal(phe_proportion(test_Prop_1$Numerator,test_Prop_1$Denominator,
                              test_Prop_100$Area, conf.level=99.8, percentage=FALSE)[1:6],
               select(test_Prop_1,c(1,2,3,5,8,9)),check.attributes=FALSE, check.names=FALSE, info="test 4")
})


# test error handling
test_that("errors are generated when invalid arguments are used",{
  expect_error(phe_proportion(-65,100),"numerators must be greater than or equal to zero", info="error test 1")
  expect_error(phe_proportion(65,-100),"denominators must be greater than zero", info="error test 1")
  expect_error(phe_proportion(65,0),"denominators must be greater than zero", info="error test 1")
  expect_error(phe_proportion(100,65),"numerators must be less than or equal to denominator for a proportion statistic", info="error test 1")
  expect_error(phe_proportion(65,100,conf.level=20),
               "confidence level must be between 90 and 100 or between 0.9 and 1", info="error test 1")
  expect_error(phe_proportion(c(5,65,90,98),c(100,100)),
               "numerator and denominator vectors must be of equal length", info="error test 1")
})

