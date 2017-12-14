library(testthat)
library(readxl)

context("test_phe_isr")

# Load test data
test_ISR_multiarea <- read_excel(".\\tests\\testthat\\testdata_ISR.xlsx", sheet="testdata_multiarea", col_names=TRUE)
test_ISR_err1 <- read_excel(".\\tests\\testthat\\testdata_ISR.xlsx", sheet="testdata_err1", col_names=TRUE)
test_ISR_err2 <- read_excel(".\\tests\\testthat\\testdata_ISR.xlsx", sheet="testdata_err2", col_names=TRUE)
test_ISR_err3 <- read_excel(".\\tests\\testthat\\testdata_ISR.xlsx", sheet="testdata_err3", col_names=TRUE)
test_ISR_results   <- read_excel(".\\tests\\testthat\\testdata_ISR.xlsx", sheet="testresults", col_names=TRUE)
refdata            <- read_excel(".\\tests\\testthat\\testdata_ISR.xlsx", sheet="refdata", col_names=TRUE)

test_ISR_results$group <- as.factor(test_ISR_results$group)

#test calculations
test_that("isrs and CIs calculate correctly",{
  expect_equal(data.frame(phe_isr(test_ISR_multiarea$count,
                       test_ISR_multiarea$pop,
                       refdata$refcount,
                       refdata$refpop,
                       groupref=test_ISR_multiarea$area))[1:6],
               select(filter(test_ISR_results,test_ISR_results$type == "ref_0"),2:7),
               check.attributes=FALSE, check.names=FALSE, info="test1")
  expect_equal(data.frame(phe_isr(test_ISR_multiarea$count,
                                  test_ISR_multiarea$pop,
                                  refdata$refcount,
                                  refdata$refpop,
                                  groupref=test_ISR_multiarea$area,
                                  ref_0 = FALSE))[1:6],
               select(filter(test_ISR_results,test_ISR_results$type == "ref_100"),2:7),
               check.attributes=FALSE, check.names=FALSE, info="test2")
  expect_equal(data.frame(phe_isr(test_ISR_multiarea$count,
                                  test_ISR_multiarea$pop,
                                  refdata$refcount,
                                  refdata$refpop,
                                  groupref=test_ISR_multiarea$area,
                                  ratio = FALSE))[1:6],
               select(filter(test_ISR_results,test_ISR_results$type == "rate_100000"),2:7),
               check.attributes=FALSE, check.names=FALSE, info="test3")
  expect_equal(data.frame(phe_isr(test_ISR_multiarea$count,
                                  test_ISR_multiarea$pop,
                                  refdata$refcount,
                                  refdata$refpop,
                                  groupref=test_ISR_multiarea$area,
                                  conf.level = 0.998))[1:6],
               select(filter(test_ISR_results,test_ISR_results$type == "ref_0"),2:5,8:9),
               check.attributes=FALSE, check.names=FALSE, info="test4")
  expect_equal(data.frame(phe_isr(test_ISR_multiarea$count,
                                  test_ISR_multiarea$pop,
                                  refdata$refcount,
                                  refdata$refpop,
                                  groupref=test_ISR_multiarea$area))[1:6],
               select(filter(test_ISR_results,test_ISR_results$type == "ref_0"),2:7),
               check.attributes=FALSE, check.names=FALSE, info="test5")

})


# test error handling
test_that("errors are generated when invalid arguments are used",{
  expect_error(data.frame(phe_isr(test_ISR_err1$count,
                                  test_ISR_err1$pop,
                                  refdata$refcount,
                                  refdata$refpop,
                                  groupref=test_ISR_err1$area))[1:6],
               "numerators must all be greater than or equal to zero", info="error test1")
  expect_error(data.frame(phe_isr(test_ISR_err2$count,
                                  test_ISR_err2$pop,
                                  refdata$refcount,
                                  refdata$refpop,
                                  groupref=test_ISR_err2$area))[1:6],
               "denominators must all be greater than zero", info="error test2")
  expect_error(data.frame(phe_isr(test_ISR_err3$count,
                                  test_ISR_err3$pop,
                                  refdata$refcount,
                                  refdata$refpop,
                                  groupref=test_ISR_err3$area))[1:6],
               "denominators must all be greater than zero", info="error test3")
  expect_error(data.frame(phe_isr(test_ISR_multiarea$count,
                                  test_ISR_multiarea$pop,
                                  select(filter(test_ISR_err1,test_ISR_err1$area == "testdata_big"),3),
                                  refdata$refpop,
                                  groupref=test_ISR_err3$area))[1:6],
               "reference numerators must all be greater than or equal to zero", info="error test4")
  expect_error(data.frame(phe_isr(test_ISR_multiarea$count,
                                  test_ISR_multiarea$pop,
                                  refdata$refcount,
                                  select(filter(test_ISR_err1,test_ISR_err1$area == "testdata_big"),4),
                                  groupref=test_ISR_err3$area))[1:6],
               "reference denominators must all be greater than zero", info="error test4")
})



#  stop("reference denominators must all be greater than zero") - not yet working.

# stop("confidence level must be between 90 and 100 or between 0.9 and 1")

# stop("numerator and denominator vectors must be of equal length")

#  stop("reference numerator and reference denominator vectors must be of equal length")

#  stop("numerator vector length must be a multiple of the reference numerator vector length")
