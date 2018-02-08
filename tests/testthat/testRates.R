context("test_phe_rate")


#test calculations
test_that("rates and CIs calculate correctly",{

  expect_equal(data.frame(phe_rate(slice(test_Rate,9:16)[1:3],Numerator,Denominator)),
               data.frame(select(slice(test_Rate,9:16),1:6)),check.attributes=FALSE, check.names=FALSE, info="test default")

  expect_equal(data.frame(phe_rate(slice(test_Rate,9:16)[1:3],Numerator,Denominator, type="full")),
               data.frame(select(slice(test_Rate,9:16),1:9)),check.attributes=FALSE, check.names=FALSE, info="test full")

  expect_equal(data.frame(phe_rate(slice(test_Rate,25:32)[1:3],Numerator,Denominator, confidence=99.8)),
               data.frame(select(slice(test_Rate,25:32),1:6)),check.attributes=FALSE, check.names=FALSE, info="test confidence")

  expect_equal(data.frame(phe_rate(slice(test_Rate,1:8)[1:3],Numerator,Denominator, multiplier=100)),
               data.frame(select(slice(test_Rate,1:8),1:6)),check.attributes=FALSE, check.names=FALSE, info="test multiplier")

  expect_equal(data.frame(phe_rate(slice(test_Rate,9:16)[1:3],Numerator,Denominator, type="value")),
               data.frame(select(slice(test_Rate,9:16),1:4)),check.attributes=FALSE, check.names=FALSE, info="test value")

  expect_equal(data.frame(phe_rate(slice(test_Rate,9:16)[1:3],Numerator,Denominator, type="lower")),
               data.frame(select(slice(test_Rate,9:16),1:3,5)),check.attributes=FALSE, check.names=FALSE, info="test lower")

  expect_equal(data.frame(phe_rate(slice(test_Rate,9:16)[1:3],Numerator,Denominator, type="upper")),
               data.frame(select(slice(test_Rate,9:16),1:3,6)),check.attributes=FALSE, check.names=FALSE, info="test upper")

})




# test error handling
test_that("rates - errors are generated when invalid arguments are used",{
  expect_error(phe_rate(data.frame(area=c("Area1","Area2","Area3"),
                                   obs =c(65,80,30),
                                   pop =c(100,100,100)), obs),
               "function phe_dsr requires at least 3 arguments: data, x, n", info="error invalid number of arguments")

  expect_error(phe_rate(data.frame(area=c("Area1","Area2","Area3"),
                                   obs =c(-65,80,30),
                                   pop =c(100,100,100)), obs, pop),
               "numerators must be greater than or equal to zero", info="error num < 0")

  expect_error(phe_rate(data.frame(area=c("Area1","Area2","Area3"),
                                   obs =c(65,80,30),
                                   pop =c(100,100,0)), obs, pop),
               "denominators must be greater than zero", info="error denom = 0")

  expect_error(phe_rate(data.frame(area=c("Area1","Area2","Area3"),
                                   obs =c(65,80,30),
                                   pop =c(100,-100,100)), obs, pop),
               "denominators must be greater than zero", info="error denom < 0")

  expect_error(phe_rate(data.frame(area=c("Area1","Area2","Area3"),
                                   obs =c(65,80,30),
                                   pop =c(100,100,100)), obs, pop, confidence = 0.5),
               "confidence level must be between 90 and 100 or between 0.9 and 1", info="error confidence < 0.9")

  expect_error(phe_rate(data.frame(area=c("Area1","Area2","Area3"),
                                   obs =c(65,80,30),
                                   pop =c(100,100,100)), obs, pop, confidence = 40),
               "confidence level must be between 90 and 100 or between 0.9 and 1", info="error confidence between 1 and 90")

  expect_error(phe_rate(data.frame(area=c("Area1","Area2","Area3"),
                                   obs =c(65,80,30),
                                   pop =c(100,100,100)), obs, pop, confidence = 9998),
               "confidence level must be between 90 and 100 or between 0.9 and 1", info="error confidence >100")

  expect_error(phe_rate(data.frame(area=c("Area1","Area2","Area3"),
                                   obs =c(65,80,30),
                                   pop =c(100,100,100)), obs, pop, type="combined"),
               "type must be one of value, lower, upper, standard or full", info="error invalid type")

})
