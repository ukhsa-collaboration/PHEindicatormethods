context("test_phe_proportion")


# test calculations
test_that("proportions and CIs calculate correctly",{

  expect_equal(phe_proportion(slice(test_Prop,1:8)[1:3], Numerator, Denominator),
               select(slice(test_Prop,1:8),1:6),check.attributes=FALSE, check.names=FALSE, info="test default")

  expect_equal(phe_proportion(slice(test_Prop,9:16)[1:3], Numerator, Denominator,
                              percentage = TRUE, type="full"),
               select(slice(test_Prop,9:16),1:9),check.attributes=FALSE, check.names=FALSE, info="test full, percentage")

  expect_equal(phe_proportion(slice(test_Prop,1:8)[1:3], Numerator, Denominator,
                              percentage=FALSE, type="full"),
               select(slice(test_Prop,1:8),1:9),check.attributes=FALSE, check.names=FALSE, info="test full")

  expect_equal(data.frame(phe_proportion(slice(test_Prop,17:24)[1:3], Numerator, Denominator,
                              type="full", confidence=99.8)),
               data.frame(select(slice(test_Prop,17:24),1:9)),check.attributes=FALSE, check.names=FALSE, info="test confidence")

  expect_equal(phe_proportion(slice(test_Prop,1:8)[1:3], Numerator, Denominator, type="value"),
               select(slice(test_Prop,1:8),1:4),check.attributes=FALSE, check.names=FALSE, info="test value")

  expect_equal(phe_proportion(slice(test_Prop,1:8)[1:3], Numerator, Denominator, type="lower"),
               select(slice(test_Prop,1:8),1:3,5),check.attributes=FALSE, check.names=FALSE, info="test lower")

  expect_equal(phe_proportion(slice(test_Prop,1:8)[1:3], Numerator, Denominator, type="upper"),
               select(slice(test_Prop,1:8),1:3,6),check.attributes=FALSE, check.names=FALSE, info="test upper")

})


# test error handling

test_that("proportions - errors are generated when invalid arguments are used",{

  expect_error(phe_proportion(data.frame(area=c("Area1","Area2","Area3"),
                                         obs =c(65,80,30),
                                         pop =c(100,100,100)), obs),
               "function phe_proportion requires at least 3 arguments: data, x, n", info="error not enough arguments")

  expect_error(phe_proportion(data.frame(area=c("Area1","Area2","Area3"),
                                         obs =c(65,-80,30),
                                         pop =c(100,100,100)), obs, pop),
               "numerators must be greater than or equal to zero", info="error num < 0")

  expect_error(phe_proportion(data.frame(area=c("Area1","Area2","Area3"),
                                         obs =c(65,80,30),
                                         pop =c(100,0,100)), obs, pop),
               "denominators must be greater than zero", info="error denom = 0")

  expect_error(phe_proportion(data.frame(area=c("Area1","Area2","Area3"),
                                         obs =c(65,80,30),
                                         pop =c(100,100,-100)), obs, pop),
               "denominators must be greater than zero", info="error denom < 0")

  expect_error(phe_proportion(data.frame(area=c("Area1","Area2","Area3"),
                                         obs =c(101,80,30),
                                         pop =c(100,100,100)), obs, pop),
               "numerators must be less than or equal to denominator for a proportion statistic", info="error ")

  expect_error(phe_proportion(data.frame(area=c("Area1","Area2","Area3"),
                                         obs =c(65,80,30),
                                         pop =c(100,100,100)), obs, pop, confidence = 0.8),
               "confidence level must be between 90 and 100 or between 0.9 and 1", info="error conf < 0.9")

  expect_error(phe_proportion(data.frame(area=c("Area1","Area2","Area3"),
                                         obs =c(65,80,30),
                                         pop =c(100,100,100)), obs, pop, confidence = 50),
               "confidence level must be between 90 and 100 or between 0.9 and 1", info="error conf between 1 and 90")

  expect_error(phe_proportion(data.frame(area=c("Area1","Area2","Area3"),
                                         obs =c(65,80,30),
                                         pop =c(100,100,100)), obs, pop, confidence = 130),
               "confidence level must be between 90 and 100 or between 0.9 and 1", info="error conf >100")

  expect_error(phe_proportion(data.frame(area=c("Area1","Area2","Area3"),
                                         obs =c(65,80,30),
                                         pop =c(100,100,100)), obs, pop, type="combined"),
               "type must be one of value, lower, upper, standard or full", info="error invalid type")

})

