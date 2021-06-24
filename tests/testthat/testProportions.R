context("test_phe_proportion")


# test calculations
test_that("proportions and CIs calculate correctly",{

  expect_equal(data.frame(select(phe_proportion(slice(test_Prop,1:8)[1:3], Numerator, Denominator),1:6,8:9)),
               data.frame(select(slice(test_Prop,1:8),1:6,9:10)),
               check.attributes=FALSE, check.names=FALSE, info="test default")

  expect_equal(data.frame(select(phe_proportion(slice(test_Prop,1:8)[1:3], Numerator, Denominator,
                                                confidence = c(0.95,0.998)),1:8,10:11)),
               data.frame(select(slice(test_Prop,1:8),1:10)),
               check.attributes=FALSE, check.names=FALSE, info="test full output 2 CIs")

  expect_equal(data.frame(select(phe_proportion(slice(test_Prop,9:16)[1:3], Numerator, Denominator,
                              multiplier = 100, type="full"),1:6,8:9)),
               data.frame(select(slice(test_Prop,9:16),1:6,9:10)),
               check.attributes=FALSE, check.names=FALSE, info="test full, percentage")

  expect_equal(data.frame(phe_proportion(slice(test_Prop,1:8)[1:3], Numerator, Denominator,
                              multiplier = 1, type="standard")),
               data.frame(select(slice(test_Prop,1:8),1:6)),
               check.attributes=FALSE, check.names=FALSE, info="test standard")

  expect_equal(data.frame(phe_proportion(slice(test_Prop,1:8)[1:3], Numerator, Denominator,
                                         multiplier = 1, confidence = c(0.95,0.998), type="standard")),
               data.frame(select(slice(test_Prop,1:8),1:8)),
               check.attributes=FALSE, check.names=FALSE, info="test standard 2 CIs")

  expect_equal(data.frame(select(phe_proportion(slice(test_Prop,1:8)[1:3], Numerator, Denominator,
                              type="full", confidence=99.8), 1:6,8:9)),
               data.frame(select(slice(test_Prop,1:8),1:4,7:10)),
               check.attributes=FALSE, check.names=FALSE, info="test confidence")

  expect_equal(data.frame(phe_proportion(slice(test_Prop,1:8)[1:3], Numerator, Denominator, type="value")),
               data.frame(select(slice(test_Prop,1:8),1:4)),
               check.attributes=FALSE, check.names=FALSE, info="test value")

  expect_equal(data.frame(phe_proportion(slice(test_Prop,1:8)[1:3], Numerator, Denominator,
                                         confidence = c(0.95,0.998), type="value")),
               data.frame(select(slice(test_Prop,1:8),1:4)),
               check.attributes=FALSE, check.names=FALSE, info="test value 2 CIs")

  expect_equal(data.frame(phe_proportion(slice(test_Prop,1:8)[1:3], Numerator, Denominator, type="lower")),
               data.frame(select(slice(test_Prop,1:8),1:3,5)),
               check.attributes=FALSE, check.names=FALSE, info="test lower")

  expect_equal(data.frame(phe_proportion(slice(test_Prop,1:8)[1:3], Numerator, Denominator,
                                         confidence = c(0.95,0.998), type="lower")),
               data.frame(select(slice(test_Prop,1:8),1:3,5,7)),
               check.attributes=FALSE, check.names=FALSE, info="test lower 2 CIs")

  expect_equal(data.frame(phe_proportion(slice(test_Prop,1:8)[1:3], Numerator, Denominator, type="upper")),
               data.frame(select(slice(test_Prop,1:8),1:3,6)),
               check.attributes=FALSE, check.names=FALSE, info="test upper")

  expect_equal(data.frame(phe_proportion(slice(test_Prop,1:8)[1:3], Numerator, Denominator,
                                         confidence = c(0.95,0.998), type="upper")),
               data.frame(select(slice(test_Prop,1:8),1:3,6,8)),
               check.attributes=FALSE, check.names=FALSE, info="test upper 2 CIs")

  expect_equal(arrange(data.frame(select(phe_proportion(filter(test_Prop,Area %in% c("Area09","Area10","Area11"))[1:3],
                                                 Numerator, Denominator, type="full"),1:6,8:9)), Area),
               arrange(data.frame(select(filter(test_Prop,Area %in% c("Area09","Area10","Area11")),1:6,9:10)), Area),
               check.attributes=FALSE, check.names=FALSE, info="test NAs")

  expect_equal(arrange(data.frame(phe_proportion(slice(test_Prop_g,1:8)[1:3], Numerator, Denominator, type="standard")), Area),
               arrange(select(data.frame(test_Prop_g_results[1:6]),1:6),Area),
               check.attributes=FALSE, check.names=FALSE, info="test grouped")

  expect_equal(data.frame(phe_proportion(slice(test_Prop,1:8)[1:3], Numerator, Denominator, confidence = c(0.95, 0.998)))[1:6],
               data.frame(select(slice(test_Prop,1:8),1:9))[1:6],
               check.attributes=FALSE, check.names=FALSE, info="test two CIs 95%")

  expect_equal(data.frame(select(phe_proportion(slice(test_Prop,9:16)[1:3], Numerator, Denominator,
                                                multiplier = 100, confidence = c(0.95, 0.998)),1:4,7,8)),
               data.frame(select(slice(test_Prop,9:16),1:4,7:8)),
               check.attributes=FALSE, check.names=FALSE, info="test two CIs 99.8%")

  expect_equal(data.frame(select(phe_proportion(slice(test_Prop,9:16)[1:3], Numerator, Denominator,
                                                confidence = c(0.95, 0.998)),9)),
               data.frame(confidence = rep("95%, 99.8%",8), stringsAsFactors = FALSE),
               check.attributes=FALSE, check.names=FALSE, info="test two CIs, metadata")

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

  expect_error(data.frame(phe_proportion(slice(test_Prop,1:8)[1:3], Numerator, Denominator, confidence = c(0.95, 0.998, 0.8))),
               "a maximum of two confidence levels can be provided", info="length confidence > 2")

  expect_error(data.frame(phe_proportion(slice(test_Prop,1:8)[1:3], Numerator, Denominator, confidence = c(0.95, 0.98))),
               "two confidence levels can only be produced if they are specified as 0.95 and 0.998",
               info="two CIs specified are not 0.95 and 0.998")

})
