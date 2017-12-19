library(testthat)
library(readxl)

context("test_phe_rate")


#test calculations
test_that("rates and CIs calculate correctly",{
  expect_equal(phe_rate(test_Rate_100$Numerator, test_Rate_100$Denominator,
                        test_Rate_100$Area, multiplier=100)[1:6],
               select(test_Rate_100,c(1,2,3,5,6,7)),check.attributes=FALSE, check.names=FALSE, info="test1")
  expect_equal(phe_rate(test_Rate_100000$Numerator,test_Rate_100000$Denominator,
                        test_Rate_100000$Area, multiplier=100000)[1:6],
               select(test_Rate_100000,c(1,2,3,5,6,7)),check.attributes=FALSE, check.names=FALSE, info="test2")
  expect_equal(phe_rate(test_Rate_100$Numerator,test_Rate_100$Denominator,
                        test_Rate_100$Area, conf.level=99.8, multiplier=100)[1:6],
               select(test_Rate_100,c(1,2,3,5,8,9)),check.attributes=FALSE, check.names=FALSE, info="test3")
  expect_equal(phe_rate(test_Rate_100000$Numerator,test_Rate_100000$Denominator,
                        test_Rate_100000$Area, conf.level=0.998, multiplier=100000)[1:6],
               select(test_Rate_100000,c(1,2,3,5,8,9)),check.attributes=FALSE, check.names=FALSE, info="test4")
})


# test error handling
test_that("errors are generated when invalid arguments are used",{
  expect_error(phe_rate(-65,100,"Area 1"),"numerators must be greater than or equal to zero", info="error test 1")
  expect_error(phe_rate(65,-100,"Area 1"),"denominators must be greater than zero", info="error test 2")
  expect_error(phe_rate(65,0,"Area 1"),"denominators must be greater than zero", info="error test 3")
  expect_error(phe_rate(65,100,"Area 1",conf.level=20),"confidence level must be between 90 and 100 or between 0.9 and 1", info="error test 4")
  expect_error(phe_rate(c(5,65,800),c(100,100,1000),"Area 1"),"numerator, denominator and row label vectors must be of equal length", info="error test 5")
})
