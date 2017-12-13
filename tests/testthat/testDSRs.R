library(testthat)
library(readxl)

# import test data
testdata_DSR_big       <- read_excel(".\\tests\\testthat\\testdata_DSR.xlsx", sheet="testdata_big",       col_names=TRUE)
#testdata_DSR_small     <- read_excel(".\\tests\\testthat\\testdata_DSR.xlsx", sheet="testdata_small",     col_names=TRUE)
#testdata_DSR_tiny      <- read_excel(".\\tests\\testthat\\testdata_DSR.xlsx", sheet="testdata_tiny",      col_names=TRUE)
testdata_DSR_multiarea <- read_excel(".\\tests\\testthat\\testdata_DSR.xlsx", sheet="testdata_multiarea", col_names=TRUE)
testdata_DSR_err1      <- read_excel(".\\tests\\testthat\\testdata_DSR.xlsx", sheet="testdata_err1",      col_names=TRUE)
testdata_DSR_err2      <- read_excel(".\\tests\\testthat\\testdata_DSR.xlsx", sheet="testdata_err2",      col_names=TRUE)
testdata_DSR_err3      <- read_excel(".\\tests\\testthat\\testdata_DSR.xlsx", sheet="testdata_err3",      col_names=TRUE)
testdata_DSR_1976      <- read_excel(".\\tests\\testthat\\testdata_DSR.xlsx", sheet="testdata_1976",     col_names=TRUE)
testdata_results       <- read_excel(".\\tests\\testthat\\testdata_DSR.xlsx", sheet="testresults",   col_names=TRUE)

testdata_results$group <- as.factor(testdata_results$group)

#test calculations
test_that("dsrs and CIs calculate correctly",{
  expect_equal(data.frame(phe_dsr(testdata_DSR_multiarea$count,
                       testdata_DSR_multiarea$pop,
                       stdpop = esp2013,
                       groupref=testdata_DSR_multiarea$area,
                       multiplier=10000)[1:6]),
               select(testdata_results,1:6), check.attributes=FALSE, check.names=FALSE,info="test1")

  expect_equal(data.frame(phe_dsr(testdata_DSR_multiarea$count,
                       testdata_DSR_multiarea$pop,
                       stdpop = esp2013,
                       groupref=testdata_DSR_multiarea$area,
                       multiplier=10000,
                       conf.level=0.998)[1:6]),
               select(testdata_results,1,2,3,4,7,8), check.attributes=FALSE, check.names=FALSE,info="test2")

  expect_equal(data.frame(phe_dsr(testdata_DSR_multiarea$count,
                       testdata_DSR_multiarea$pop,
                       stdpop = esp2013,
                       groupref=testdata_DSR_multiarea$area,
                       multiplier=10000,
                       conf.level=95)[1:6]),
               select(testdata_results,1:6), check.attributes=FALSE, check.names=FALSE,info="test3")

  expect_equal(data.frame(phe_dsr(testdata_DSR_big$count,
                       testdata_DSR_big$pop,
                       stdpop = esp2013)[2:6]),
               select(filter(testdata_results,group=="testdata_big"),2:6)*c(1,1,10,10,10), check.attributes=FALSE, check.names=FALSE,info="test4")
})

# test error handling
test_that("errors are generated when invalid arguments are used",{
  expect_error(data.frame(phe_dsr(testdata_DSR_err1$count,
                                  testdata_DSR_err1$pop,
                                  stdpop = esp2013,
                                  groupref=testdata_DSR_err1$area)),
               "numerators must all be greater than or equal to zero")
  expect_error(data.frame(phe_dsr(testdata_DSR_err2$count,
                                  testdata_DSR_err2$pop,
                                  stdpop = esp2013,
                                  groupref=testdata_DSR_err2$area)),
               "denominators must all be greater than zero")
  expect_error(data.frame(phe_dsr(testdata_DSR_err3$count,
                                  testdata_DSR_err3$pop,
                                  stdpop = esp2013,
                                  groupref=testdata_DSR_err3$area)),
               "denominators must all be greater than zero")
  expect_error(data.frame(phe_dsr(testdata_DSR_multiarea$count,
                                  testdata_DSR_multiarea$pop,
                                  stdpop = esp2013,
                                  groupref=testdata_DSR_multiarea$area,
                                  conf.level=50)),
               "confidence level must be >= 90 and <= 100 (or >= 0.9 and <= 1)")
  expect_error(data.frame(phe_dsr(testdata_DSR_multiarea$count,
                                  testdata_DSR_multiarea$pop[1:30],
                                  stdpop = esp2013,
                                  groupref=testdata_DSR_multiarea$area)),
               "numerator and denominator vectors must be of equal length")
  expect_error(data.frame(phe_dsr(testdata_DSR_multiarea$count,
                                  testdata_DSR_multiarea$pop,
                                  stdpop = testdata_DSR_1976$stdpop,
                                  groupref=testdata_DSR_multiarea$area)),
               "numerator vector length must be a multiple of standard population vector length")
})







# ?? check out dsrTest to check whether Dobson method was used.
#dobsonControl()
#dsrTest()
