library(testthat)
library(readxl)

context("test_phe_dsr")

#test calculations
test_that("dsrs and CIs calculate correctly",{
  expect_equal(data.frame(phe_dsr(test_DSR_multiarea, count, pop,
                                  type="full", stdpop = esp2013, multiplier=10000)[1:6]),
               select(filter(test_DSR_results,group!="testdata_1976"),1:6),
               check.attributes=FALSE, check.names=FALSE,info="test1")

  expect_equal(data.frame(phe_dsr(test_DSR_multiarea, count, pop,
                                  type="full", stdpop = esp2013, multiplier=10000, conf.level=0.998)[1:6]),
               select(filter(test_DSR_results,group!="testdata_1976"),1:4,7:8),
               check.attributes=FALSE, check.names=FALSE,info="test2")

  expect_equal(data.frame(phe_dsr(test_DSR_multiarea, count, pop,
                                  stdpop = esp2013, multiplier=10000, conf.level=95)),
               select(filter(test_DSR_results,group!="testdata_1976"),1,4:6),
               check.attributes=FALSE, check.names=FALSE,info="test3")

  expect_equal(data.frame(phe_dsr(test_DSR_1976, count, pop,
                                  type="full", stdpop = test_DSR_1976$stdpop)[1:5]),
               select(filter(test_DSR_results,group=="testdata_1976"),2:6),
               check.attributes=FALSE, check.names=FALSE,info="test4")

  expect_equal(data.frame(phe_dsr(test_DSR_multiarea, count, pop,
                                  type="value", stdpop = esp2013)),
               select(filter(test_DSR_results,group!="testdata_1976"),1,4) %>%
                 mutate(dsr = dsr*10),
               check.attributes=FALSE, check.names=FALSE,info="test1")


})

# test error handling
test_that("dsrs - errors are generated when invalid arguments are used",{
   expect_error(phe_dsr(count, pop, esp2013),
                "function phe_dsr requires at least 4 arguments: data, x, n, stdpop",info="error test 1")

   expect_error(phe_dsr(test_DSR_err1, count, pop, test_DSR_er1$stdpop),
                "numerators must all be greater than or equal to zero",info="error test 2")

   expect_error(phe_dsr(test_DSR_err2, count, pop, test_DSR_er1$stdpop),
                "denominators must all be greater than zero",info="error test 3")

   expect_error(phe_dsr(test_DSR_err3, count, pop, test_DSR_er1$stdpop),
                "denominators must all be greater than zero",info="error test 4")

   expect_error(phe_dsr(test_DSR_multiarea, count, pop, esp2013, conf.level=86),
                "confidence level must be between 90 and 100 or between 0.9 and 1",info="error test 5")

   expect_error(phe_dsr(test_DSR_multiarea, count, pop, esp2013, type="fill"),
                "type must be one of value, lower, upper, combined or full",info="error test 6")
# not working
   expect_error(phe_dsr(ungroup(test_DSR_multiarea), count, pop, esp2013, type="full"),
                "data must contain the same number of rows for each group",info="error test 7")
# not working
   expect_error(phe_dsr(test_DSR_multiarea, count, pop, test_DSR_1976$stdpop, type="full"),
                "stdpop length must equal number of rows in each group within data",info="error test 8")

})
