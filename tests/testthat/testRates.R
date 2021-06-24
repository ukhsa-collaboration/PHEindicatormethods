context("test_phe_rate")


#test calculations
test_that("rates and CIs calculate correctly",{

  expect_equal(data.frame(select(phe_rate(slice(test_Rate,9:16)[1:3],Numerator,Denominator),1:6,8:9)),
               data.frame(select(slice(test_Rate,9:16),1:6,9:10)),
               check.attributes=FALSE, check.names=FALSE, info="test default")

  expect_equal(data.frame(select(phe_rate(slice(test_Rate,9:16)[1:3],
                                          Numerator,Denominator, confidence = c(0.95,0.998)),1:8,10:11)),
               data.frame(select(slice(test_Rate,9:16),1:10)),
               check.attributes=FALSE, check.names=FALSE, info="test full output 2 CIs")

  expect_equal(data.frame(phe_rate(slice(test_Rate,9:16)[1:3],Numerator,Denominator, type="standard")),
               data.frame(select(slice(test_Rate,9:16),1:6)),
               check.attributes=FALSE, check.names=FALSE, info="test standard")

  expect_equal(data.frame(phe_rate(slice(test_Rate,9:16)[1:3],Numerator,Denominator,
                                   confidence = c(0.95,0.998), type="standard")),
               data.frame(select(slice(test_Rate,9:16),1:8)),
               check.attributes=FALSE, check.names=FALSE, info="test standard 2 CIs")

  expect_equal(data.frame(phe_rate(slice(test_Rate,20)[1:3],Numerator,Denominator, type="standard")),
               data.frame(select(slice(test_Rate,20),1:6)),
               check.attributes=FALSE, check.names=FALSE, info="test num > denom")

  expect_equal(data.frame(select(phe_rate(slice(test_Rate,9:16)[1:3],Numerator,Denominator, confidence=99.8),1:6,8:9)),
               data.frame(select(slice(test_Rate,9:16),1:4,7:10)),
               check.attributes=FALSE, check.names=FALSE, info="test confidence")

  expect_equal(data.frame(select(phe_rate(slice(test_Rate,1:8)[1:3],Numerator,Denominator, multiplier=100),1:6,8:9)),
               data.frame(select(slice(test_Rate,1:8),1:6,9:10)),
               check.attributes=FALSE, check.names=FALSE, info="test multiplier")

  expect_equal(data.frame(phe_rate(slice(test_Rate,9:16)[1:3],Numerator,Denominator, type="value")),
               data.frame(select(slice(test_Rate,9:16),1:4)),
               check.attributes=FALSE, check.names=FALSE, info="test value")

  expect_equal(data.frame(phe_rate(slice(test_Rate,9:16)[1:3],Numerator,Denominator,
                                   confidence = c(0.95,0.998), type="value")),
               data.frame(select(slice(test_Rate,9:16),1:4)),
               check.attributes=FALSE, check.names=FALSE, info="test value 2 CIs")

  expect_equal(data.frame(phe_rate(slice(test_Rate,9:16)[1:3],Numerator,Denominator, type="lower")),
               data.frame(select(slice(test_Rate,9:16),1:3,5)),
               check.attributes=FALSE, check.names=FALSE, info="test lower")

  expect_equal(data.frame(phe_rate(slice(test_Rate,9:16)[1:3],Numerator,Denominator,
                                   confidence = c(0.95,0.998), type="lower")),
               data.frame(select(slice(test_Rate,9:16),1:3,5,7)),
               check.attributes=FALSE, check.names=FALSE, info="test lower 2 CIs")

  expect_equal(data.frame(phe_rate(slice(test_Rate,9:16)[1:3],Numerator,Denominator, type="upper")),
               data.frame(select(slice(test_Rate,9:16),1:3,6)),
               check.attributes=FALSE, check.names=FALSE, info="test upper")

  expect_equal(data.frame(phe_rate(slice(test_Rate,9:16)[1:3],Numerator,Denominator,
                                   confidence = c(0.95,0.998), type="upper")),
               data.frame(select(slice(test_Rate,9:16),1:3,6,8)),
               check.attributes=FALSE, check.names=FALSE, info="test upper 2 CIs")

  expect_equal(data.frame(select(phe_rate(slice(test_Rate,17:19)[1:3],
                                          Numerator,Denominator, type="full"),1:6,8:9)),
               data.frame(select(slice(test_Rate,17:19),1:6,9:10)),
               check.attributes=FALSE, check.names=FALSE, info="test NAs")

  expect_equal(arrange(data.frame(phe_rate(slice(test_Rate_g,1:8)[1:3], Numerator, Denominator)),Area),
               arrange(data.frame(select(test_Rate_g_results,1:9)),Area),
               check.attributes=FALSE, check.names=FALSE, info="test grouped")

  expect_equal(data.frame(select(phe_rate(slice(test_Rate,9:16)[1:3],Numerator,Denominator, confidence = c(0.95, 0.998)),1:6)),
               data.frame(select(slice(test_Rate,9:16),1:6)),
               check.attributes=FALSE, check.names=FALSE, info="test two CIs 95%")

  expect_equal(data.frame(select(phe_rate(slice(test_Rate,25:32)[1:3],Numerator,Denominator, confidence = c(0.95, 0.998)),1:4,7,8)),
               data.frame(select(slice(test_Rate,25:32),1:6)),
               check.attributes=FALSE, check.names=FALSE, info="test two CIs 99.8%")

  expect_equal(data.frame(select(phe_rate(slice(test_Rate,9:16)[1:3],Numerator,Denominator, confidence = c(0.95, 0.998)),9)),
               data.frame(confidence = rep("95%, 99.8%",8), stringsAsFactors = FALSE),
               check.attributes=FALSE, check.names=FALSE, info="test two CIs metadata")
})




# test error handling
test_that("rates - errors are generated when invalid arguments are used",{
  expect_error(phe_rate(data.frame(area=c("Area1","Area2","Area3"),
                                   obs =c(65,80,30),
                                   pop =c(100,100,100)), obs),
               "function phe_rate requires at least 3 arguments: data, x, n", info="error invalid number of arguments")

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

  expect_error(data.frame(phe_rate(slice(test_Rate,9:16)[1:3],Numerator,Denominator, confidence = c(0.95, 0.998, 1.00))),
               "a maximum of two confidence levels can be provided", info="test more than 2 CIs requested")

  expect_error(data.frame(phe_rate(slice(test_Rate,9:16)[1:3],Numerator,Denominator, confidence = c(0.95, 0.98))),
               "two confidence levels can only be produced if they are specified as 0.95 and 0.998", info="test default")

})
