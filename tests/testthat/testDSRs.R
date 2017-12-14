library(testthat)
library(readxl)

context("test_phe_dsr")

# import test data
test_DSR_multiarea <- read_excel(".\\tests\\testthat\\testdata_DSR.xlsx", sheet="testdata_multiarea", col_names=TRUE)
test_DSR_1976      <- read_excel(".\\tests\\testthat\\testdata_DSR.xlsx", sheet="testdata_1976",     col_names=TRUE)
test_DSR_err1      <- read_excel(".\\tests\\testthat\\testdata_DSR.xlsx", sheet="testdata_err1",      col_names=TRUE)
test_DSR_err2      <- read_excel(".\\tests\\testthat\\testdata_DSR.xlsx", sheet="testdata_err2",      col_names=TRUE)
test_DSR_err3      <- read_excel(".\\tests\\testthat\\testdata_DSR.xlsx", sheet="testdata_err3",      col_names=TRUE)
test_DSR_results       <- read_excel(".\\tests\\testthat\\testdata_DSR.xlsx", sheet="testresults",   col_names=TRUE)

test_DSR_results$group <- as.factor(test_DSR_results$group)

#test calculations
test_that("dsrs and CIs calculate correctly",{
  expect_equal(data.frame(phe_dsr(test_DSR_multiarea$count,
                       test_DSR_multiarea$pop,
                       stdpop = esp2013,
                       groupref=test_DSR_multiarea$area,
                       multiplier=10000)[1:6]),
               select(filter(test_DSR_results,group!="testdata_1976"),1:6),
               check.attributes=FALSE, check.names=FALSE,info="test1")

  expect_equal(data.frame(phe_dsr(test_DSR_multiarea$count,
                       test_DSR_multiarea$pop,
                       stdpop = esp2013,
                       groupref=test_DSR_multiarea$area,
                       multiplier=10000,
                       conf.level=0.998)[1:6]),
               select(filter(test_DSR_results,group!="testdata_1976"),1:4,7:8),
               check.attributes=FALSE, check.names=FALSE,info="test2")

  expect_equal(data.frame(phe_dsr(test_DSR_multiarea$count,
                       test_DSR_multiarea$pop,
                       stdpop = esp2013,
                       groupref=test_DSR_multiarea$area,
                       multiplier=10000,
                       conf.level=95)[1:6]),
               select(filter(test_DSR_results,group!="testdata_1976"),1:6),
               check.attributes=FALSE, check.names=FALSE,info="test3")

  expect_equal(data.frame(phe_dsr(test_DSR_1976$count,
                       test_DSR_1976$pop,
                       stdpop = test_DSR_1976$stdpop)[2:6]),
               select(filter(test_DSR_results,group=="testdata_1976"),2:6),
               check.attributes=FALSE, check.names=FALSE,info="test4")
})

# test error handling
test_that("errors are generated when invalid arguments are used",{
  expect_error(data.frame(phe_dsr(test_DSR_err1$count,
                                  test_DSR_err1$pop,
                                  stdpop = esp2013,
                                  groupref=test_DSR_err1$area)),
               "numerators must all be greater than or equal to zero",info="error test 1")
  expect_error(data.frame(phe_dsr(test_DSR_err2$count,
                                  test_DSR_err2$pop,
                                  stdpop = esp2013,
                                  groupref=test_DSR_err2$area)),
               "denominators must all be greater than zero",info="error test 2")
  expect_error(data.frame(phe_dsr(test_DSR_err3$count,
                                  test_DSR_err3$pop,
                                  stdpop = esp2013,
                                  groupref=test_DSR_err3$area)),
               "denominators must all be greater than zero",info="error test 3")
  expect_error(data.frame(phe_dsr(test_DSR_multiarea$count,
                                  test_DSR_multiarea$pop,
                                  stdpop = esp2013,
                                  groupref=test_DSR_multiarea$area,
                                  conf.level=50)),
               "confidence level must be between 90 and 100 or between 0.9 and 1",info="error test 4")
  expect_error(data.frame(phe_dsr(test_DSR_multiarea$count,
                                  test_DSR_multiarea$pop[1:30],
                                  stdpop = esp2013,
                                  groupref=test_DSR_multiarea$area)),
               "numerator and denominator vectors must be of equal length",info="error test 5")
  expect_error(data.frame(phe_dsr(test_DSR_multiarea$count,
                                  test_DSR_multiarea$pop,
                                  stdpop = test_DSR_1976$stdpop,
                                  groupref=test_DSR_multiarea$area)),
               "numerator vector length must be a multiple of standard population vector length",info="error test 6")
})
