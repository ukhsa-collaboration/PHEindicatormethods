library(testthat)
library(readxl)

context("test_phe_mean")

#?? test1 gives error when run through devtools but not when run code below directly ???

#test calculations
test_that("means and CIs calculate correctly",{
  expect_equal(data.frame(phe_mean(test_Mean$values))[1:7],
               select(filter(test_Mean_results,area == "No grouping"),1:7),
               check.attributes=FALSE, check.names=FALSE,info="test1")
  expect_equal(data.frame(phe_mean(test_Mean$values,groupref = test_Mean$area))[1:7],
               select(filter(test_Mean_results,area != "No grouping"),1:7),
               check.attributes=FALSE, check.names=FALSE,info="test2")
})

#test error handling
test_that("means - errors are generated when invalid arguments are used",{
  expect_error(data.frame(phe_mean(test_Mean$values,conf.level=12)),
               "confidence level must be between 90 and 100 or between 0.9 and 1",info="error test 1")
})
