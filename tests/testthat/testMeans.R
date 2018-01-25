library(testthat)
library(readxl)

context("test_phe_mean")

#test calculations
test_that("means and CIs calculate correctly",{
  expect_equal(phe_mean(test_Mean,values),
               select(slice(test_Mean_results,3),5:7),check.attributes=FALSE, check.names=FALSE,info="test default")

  expect_equal(phe_mean(test_Mean_Grp,values),
               select(slice(test_Mean_results,1:2),1,5:7),check.attributes=FALSE, check.names=FALSE,info="test default grouped")

  expect_equal(phe_mean(test_Mean_Grp,values, type="full"),
               slice(test_Mean_results,1:2),check.attributes=FALSE, check.names=FALSE,info="test full")

  expect_equal(phe_mean(test_Mean_Grp,values, type="value"),
               select(slice(test_Mean_results,1:2),1,5),check.attributes=FALSE, check.names=FALSE,info="test value")

  expect_equal(phe_mean(test_Mean_Grp,values, type="lower"),
               select(slice(test_Mean_results,1:2),1,6),check.attributes=FALSE, check.names=FALSE,info="test lower")

  expect_equal(phe_mean(test_Mean_Grp,values, type="upper"),
               select(slice(test_Mean_results,1:2),1,7),check.attributes=FALSE, check.names=FALSE,info="test upper")

  expect_equal(phe_mean(test_Mean_Grp,values, confidence = 99.8),
               select(slice(test_Mean_results,4:5),1,5:7),check.attributes=FALSE, check.names=FALSE,info="test confidence")
})


#type must be one of value, lower, upper, standard or full


#test error handling

test_that("means - errors are generated when invalid arguments are used",{
  expect_error(phe_mean(test_Mean),
               "function phe_dsr requires at least 2 arguments: data, x",info="error invalid number of arguments")
  expect_error(phe_mean(test_Mean, values, confidence = 0.2),
               "confidence level must be between 90 and 100 or between 0.9 and 1",info="error confidence < 0.9")
  expect_error(phe_mean(test_Mean, values, confidence = 202),
               "confidence level must be between 90 and 100 or between 0.9 and 1",info="error confidence between 1 and 90")
  expect_error(phe_mean(test_Mean, values, confidence = 200),
               "confidence level must be between 90 and 100 or between 0.9 and 1",info="error confidence > 100")
  expect_error(phe_mean(test_Mean, values, type="combined"),
               "type must be one of value, lower, upper, standard or full",info="error invalid type")


})
