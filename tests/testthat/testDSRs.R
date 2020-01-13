context("test_phe_dsr")

#test calculations
test_that("dsrs and CIs calculate correctly",{

  expect_equal(data.frame(select(phe_dsr(test_multiarea, count, pop),1:7)),
               data.frame(test_DSR_results[5:7,1:7]),
               check.attributes=FALSE, check.names=FALSE,info="test default")

  expect_equal(data.frame(select(phe_dsr(test_multiarea, count, pop, confidence = c(0.95,0.998)),1:9,11:12)),
               data.frame(test_DSR_results[5:7,]),
               check.attributes=FALSE, check.names=FALSE,info="test full output with 2 CIs")

  expect_equal(data.frame(phe_dsr(test_DSR_1976, count, pop, stdpop = test_DSR_1976$esp1976, type="standard")),
               select(slice(test_DSR_results,8),2:3,5:7),
               check.attributes=FALSE, check.names=FALSE,info="test with user specified vector")

  expect_equal(data.frame(phe_dsr(test_DSR_1976, count, pop, stdpop = esp1976, stdpoptype="field", type="standard")),
               data.frame(select(slice(test_DSR_results,8),2:3,5:7)),
               check.attributes=FALSE, check.names=FALSE,info="test with user specified stdpop by col name")

  expect_equal(data.frame(phe_dsr(test_multiarea, count, pop, type="standard",
               stdpop = c(5000, 5500, 5500, 5500, 6000, 6000, 6500, 7000, 7000, 7000, 7000, 6500, 6000, 5500, 5000, 4000, 2500, 1500, 1000))),
               data.frame(select(slice(test_DSR_results,5:7),1:3,5:7)),
               check.attributes=FALSE, check.names=FALSE,info="test stdpop as specified vector")

  expect_equal(data.frame(phe_dsr(test_multiarea, count, pop, stdpop = esp2013, type="standard")),
               data.frame(select(slice(test_DSR_results,5:7),1:3,5:7)),
               check.attributes=FALSE, check.names=FALSE,info="test standard")

  expect_equal(data.frame(phe_dsr(test_multiarea, count, pop, confidence = c(0.95,0.998), type="standard")),
               data.frame(select(slice(test_DSR_results,5:7),1:3,5:9)),
               check.attributes=FALSE, check.names=FALSE,info="test standard 2CIs")

  expect_equal(data.frame(phe_dsr(test_multiarea, count, pop, type="value")),
               data.frame(select(slice(test_DSR_results,5:7),1,5)),
               check.attributes=FALSE, check.names=FALSE,info="test value")

  expect_equal(data.frame(phe_dsr(test_multiarea, count, pop, confidence = c(0.95,0.998), type="value")),
               data.frame(select(slice(test_DSR_results,5:7),1,5)),
               check.attributes=FALSE, check.names=FALSE,info="test value 2CIs")

  expect_equal(data.frame(phe_dsr(test_multiarea, count, pop, type="lower")),
               data.frame(select(slice(test_DSR_results,5:7),1,6)),
               check.attributes=FALSE, check.names=FALSE,info="test lower")

  expect_equal(data.frame(phe_dsr(test_multiarea, count, pop, confidence = c(0.95,0.998), type="lower")),
               data.frame(select(slice(test_DSR_results,5:7),1,6,8)),
               check.attributes=FALSE, check.names=FALSE,info="test lower 2 CIs")

  expect_equal(data.frame(phe_dsr(test_multiarea, count, pop, type="upper")),
               data.frame(select(slice(test_DSR_results,5:7),1,7)),
               check.attributes=FALSE, check.names=FALSE,info="test upper")

  expect_equal(data.frame(phe_dsr(test_multiarea, count, pop, confidence = c(0.95,0.998), type="upper")),
               data.frame(select(slice(test_DSR_results,5:7),1,7,9)),
               check.attributes=FALSE, check.names=FALSE,info="test upper 2 CIs")

  expect_equal(data.frame(select(phe_dsr(test_multiarea, count, pop, stdpop = esp2013, confidence = 99.8),1:7,9:10)),
               data.frame(select(slice(test_DSR_results,5:7),1:5,8:11)),
               check.attributes=FALSE, check.names=FALSE,info="test confidence")

  expect_equal(data.frame(phe_dsr(test_multiarea, count, pop, stdpop = esp2013, multiplier=10000, type="standard")),
               data.frame(select(slice(test_DSR_results,1:3),1:3,5:7)),
               check.attributes=FALSE, check.names=FALSE,info="test multiplier")

  expect_equal(data.frame(select(phe_dsr(test_multiarea, count, pop, confidence = c(0.95, 0.998)),10)),
               data.frame(confidence = c("95%, 99.8%","95%, 99.8%","95%, 99.8%"), stringsAsFactors=FALSE),
               check.attributes=FALSE, check.names=FALSE,info="test 2 CIs metadata")

  expect_equal(data.frame(select(phe_dsr(test_multiarea, count, pop),8)),
               data.frame(confidence = c("95%", "95%", "95%"), stringsAsFactors=FALSE),
               check.attributes=FALSE, check.names=FALSE,info="test 95% CI metadata")

  expect_equal(data.frame(select(phe_dsr(test_multiarea, count, pop, confidence = 0.998),8)),
               data.frame(confidence = c("99.8%", "99.8%", "99.8%"), stringsAsFactors=FALSE),
               check.attributes=FALSE, check.names=FALSE,info="test 99.8% CI metadata")

})




# test error handling

test_that("dsrs - errors are generated when invalid arguments are used",{

  expect_error(phe_dsr(test_multiarea, count),
                "function phe_dsr requires at least 3 arguments: data, x, n",info="error invalid number of arguments")

  expect_error(phe_dsr(test_err1, count, pop, stdpop = esp2013),
               "numerators must all be greater than or equal to zero",info="error numerators < 0")

  expect_error(phe_dsr(test_err2, count, pop, stdpop = esp2013),
               "denominators must all be greater than zero",info="error denominator = 0")

  expect_error(phe_dsr(test_err3, count, pop, stdpop = esp2013),
               "denominators must all be greater than zero",info="error denominator < 0")

  expect_error(phe_dsr(test_multiarea, count, pop, stdpop = esp2013, confidence = 0.74),
               "confidence level must be between 90 and 100 or between 0.9 and 1",info="error confidence < 0.9")

  expect_error(phe_dsr(test_multiarea, count, pop, stdpop = esp2013, confidence = 3),
               "confidence level must be between 90 and 100 or between 0.9 and 1",info="error confidence between 1 and 90")

  expect_error(phe_dsr(test_multiarea, count, pop, stdpop = esp2013, confidence = 1000),
               "confidence level must be between 90 and 100 or between 0.9 and 1",info="error confidence >100")

  expect_error(phe_dsr(test_multiarea, count, pop, stdpop = esp2013, type="combined"),
               "type must be one of value, lower, upper, standard or full",info="error invalid type")

  expect_error(phe_dsr(filter(test_multiarea,count < 100), count, pop, stdpop = esp2013),
               "data must contain the same number of rows for each group",info="error num rows per group")

  expect_error(phe_dsr(test_multiarea, count, pop, stdpop = test_DSR_1976$esp1976),
               "stdpop length must equal number of rows in each group within data",info="error stdpop length")

  expect_error(phe_dsr(test_multiarea, count, pop, stdpoptype = "column"),
               "valid values for stdpoptype are vector and field",info="error stdpoptype")

  expect_error(phe_dsr(test_DSR_1976, count, pop, stdpoptype = "field", stdpop = esp),
               "stdpop is not a field name from data",info="error stdpop field doesn't exist")

  expect_error(phe_dsr(test_multiarea, count, pop, confidence = c(90.95, 0.998, 1.00)),
               "a maximum of two confidence levels can be provided",info="error more than 2 CIs requested")

  expect_error(phe_dsr(test_multiarea, count, pop, confidence = c(0.95, 1.00)),
               "two confidence levels can only be produced if they are specified as 0.95 and 0.998",
               info="error 2 CIs aren't 0.95 and 0.998")
})


