context("test_phe_mean")

#test calculations
test_that("means and CIs calculate correctly",{
  expect_equal(data.frame(select(phe_mean(test_Mean,values),1:6,8:9)),
               data.frame(select(slice(test_Mean_results,3),2:7,10:11)),
               check.attributes=FALSE, check.names=FALSE,info="test default")

  expect_equal(data.frame(select(phe_mean(test_Mean,values, confidence = c(0.95,0.998)),1:8,10:11)),
               data.frame(select(slice(test_Mean_results,3),2:11)),
               check.attributes=FALSE, check.names=FALSE,info="test full output 2 CIs")

  expect_equal(data.frame(phe_mean(test_Mean_Grp,values, type="standard")),
                          data.frame(select(slice(test_Mean_results,1:2),1:7)),
               check.attributes=FALSE, check.names=FALSE,info="test grouped & standard")

  expect_equal(data.frame(phe_mean(test_Mean_Grp,values, confidence = c(0.95,0.998), type="standard")),
               data.frame(select(slice(test_Mean_results,1:2),1:9)),
               check.attributes=FALSE, check.names=FALSE,info="test grouped & standard 2 CIs")

  expect_equal(data.frame(phe_mean(test_Mean_Grp,values, type="value")),
               data.frame(select(slice(test_Mean_results,1:2),1,5)),
               check.attributes=FALSE, check.names=FALSE,info="test value")

  expect_equal(data.frame(phe_mean(test_Mean_Grp,values, confidence = c(0.95,0.998), type="value")),
               data.frame(select(slice(test_Mean_results,1:2),1,5)),
               check.attributes=FALSE, check.names=FALSE,info="test value 2 CIs")

  expect_equal(data.frame(phe_mean(test_Mean_Grp,values, type="lower")),
               data.frame(select(slice(test_Mean_results,1:2),1,6)),
               check.attributes=FALSE, check.names=FALSE,info="test lower")

  expect_equal(data.frame(phe_mean(test_Mean_Grp,values, confidence = c(0.95,0.998), type="lower")),
               data.frame(select(slice(test_Mean_results,1:2),1,6,8)),
               check.attributes=FALSE, check.names=FALSE,info="test lower 2 CIs")

  expect_equal(data.frame(phe_mean(test_Mean_Grp,values, type="upper")),
               data.frame(select(slice(test_Mean_results,1:2),1,7)),
               check.attributes=FALSE, check.names=FALSE,info="test upper")

  expect_equal(data.frame(phe_mean(test_Mean_Grp,values, confidence = c(0.95,0.998), type="upper")),
               data.frame(select(slice(test_Mean_results,1:2),1,7,9)),
               check.attributes=FALSE, check.names=FALSE,info="test upper 2 CIs")

  expect_equal(data.frame(phe_mean(test_Mean_Grp,values, confidence = 99.8, type="standard")),
                          data.frame(select(slice(test_Mean_results,1:2),1:5,8:9)),
               check.attributes=FALSE, check.names=FALSE,info="test confidence")

  expect_equal(data.frame(select(phe_mean(test_Mean,values, confidence = c(0.95, 0.998)),1:6)),
               data.frame(select(slice(test_Mean_results,3),2:7)),
               check.attributes=FALSE, check.names=FALSE,info="test two CIS 95%")

  expect_equal(data.frame(select(phe_mean(test_Mean_Grp,values, confidence = c(0.95, 0.998)),1:5,8,9)),
               data.frame(select(slice(test_Mean_results,1:2),1:5,8:9)),
               check.attributes=FALSE, check.names=FALSE,info="test two CIS 99.8%")

  expect_equal(data.frame(select(phe_mean(test_Mean,values, confidence = c(0.95, 0.998)),9)),
               data.frame(confidence = "95%, 99.8%", stringsAsFactors=FALSE),
               check.attributes=FALSE, check.names=FALSE,info="test two CIS metadata")

})


#test error handling
test_that("means - errors are generated when invalid arguments are used",{
  expect_error(phe_mean(test_Mean),
               "function phe_mean requires at least 2 arguments: data, x",info="error invalid number of arguments")
  expect_error(phe_mean(test_Mean, values, confidence = 0.2),
               "confidence level must be between 90 and 100 or between 0.9 and 1",info="error confidence < 0.9")
  expect_error(phe_mean(test_Mean, values, confidence = 202),
               "confidence level must be between 90 and 100 or between 0.9 and 1",info="error confidence between 1 and 90")
  expect_error(phe_mean(test_Mean, values, confidence = 200),
               "confidence level must be between 90 and 100 or between 0.9 and 1",info="error confidence > 100")
  expect_error(phe_mean(test_Mean, values, type="combined"),
               "type must be one of value, lower, upper, standard or full",info="error invalid type")
  expect_error(phe_mean(test_Mean, values, confidence = c(0.95, 0.98, 0.98)),
               "a maximum of two confidence levels can be provided",info="error more than 2 CIs specified")
  expect_error(phe_mean(test_Mean, values, confidence = c(0.95, 1.00)),
               "two confidence levels can only be produced if they are specified as 0.95 and 0.998",info="error 2 CIs not valid 95 and 99.8")


})
