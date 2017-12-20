library(testthat)
library(readxl)

context("test_phe_isr")

#test calculations
test_that("isrs and CIs calculate correctly",{
  expect_equal(data.frame(phe_isr(test_ISR_multiarea$count,
                       test_ISR_multiarea$pop,
                       test_ISR_refdata$refcount,
                       test_ISR_refdata$refpop,
                       groupref=test_ISR_multiarea$area))[1:6],
               select(filter(test_ISR_results,test_ISR_results$type == "ref_0"),2:7),
               check.attributes=FALSE, check.names=FALSE, info="test1")
  expect_equal(data.frame(phe_isr(test_ISR_multiarea$count,
                                  test_ISR_multiarea$pop,
                                  test_ISR_refdata$refcount,
                                  test_ISR_refdata$refpop,
                                  groupref=test_ISR_multiarea$area,
                                  ref_0 = FALSE))[1:6],
               select(filter(test_ISR_results,test_ISR_results$type == "ref_100"),2:7),
               check.attributes=FALSE, check.names=FALSE, info="test2")
  expect_equal(data.frame(phe_isr(test_ISR_multiarea$count,
                                  test_ISR_multiarea$pop,
                                  test_ISR_refdata$refcount,
                                  test_ISR_refdata$refpop,
                                  groupref=test_ISR_multiarea$area,
                                  ratio = FALSE))[1:6],
               select(filter(test_ISR_results,test_ISR_results$type == "rate_100000"),2:7),
               check.attributes=FALSE, check.names=FALSE, info="test3")
  expect_equal(data.frame(phe_isr(test_ISR_multiarea$count,
                                  test_ISR_multiarea$pop,
                                  test_ISR_refdata$refcount,
                                  test_ISR_refdata$refpop,
                                  groupref=test_ISR_multiarea$area,
                                  conf.level = 0.998))[1:6],
               select(filter(test_ISR_results,test_ISR_results$type == "ref_0"),2:5,8:9),
               check.attributes=FALSE, check.names=FALSE, info="test4")
  expect_equal(data.frame(phe_isr(test_ISR_multiarea$count,
                                  test_ISR_multiarea$pop,
                                  test_ISR_refdata$refcount,
                                  test_ISR_refdata$refpop,
                                  groupref=test_ISR_multiarea$area))[1:6],
               select(filter(test_ISR_results,test_ISR_results$type == "ref_0"),2:7),
               check.attributes=FALSE, check.names=FALSE, info="test5")

})


# test error handling
test_that("isrs - errors are generated when invalid arguments are used",{
  expect_error(data.frame(phe_isr(test_ISR_err1$count,
                                  test_ISR_err1$pop,
                                  test_ISR_refdata$refcount,
                                  test_ISR_refdata$refpop,
                                  groupref=test_ISR_err1$area)),
               "numerators must all be greater than or equal to zero", info="error test1")
  expect_error(data.frame(phe_isr(test_ISR_err2$count,
                                  test_ISR_err2$pop,
                                  test_ISR_refdata$refcount,
                                  test_ISR_refdata$refpop,
                                  groupref=test_ISR_err2$area)),
               "denominators must all be greater than zero", info="error test2")
  expect_error(data.frame(phe_isr(test_ISR_err3$count,
                                  test_ISR_err3$pop,
                                  test_ISR_refdata$refcount,
                                  test_ISR_refdata$refpop,
                                  groupref=test_ISR_err3$area)),
               "denominators must all be greater than zero", info="error test3")
  expect_error(data.frame(phe_isr(test_ISR_multiarea$count,
                                  test_ISR_multiarea$pop,
                                  select(filter(test_ISR_err1,test_ISR_err1$area == "testdata_big"),3),
                                  test_ISR_refdata$refpop,
                                  groupref=test_ISR_err3$area)),
               "reference numerators must all be greater than or equal to zero", info="error test4")
  expect_error(data.frame(phe_isr(test_ISR_multiarea$count,
                                  test_ISR_multiarea$pop,
                                  test_ISR_refdata$refcount,
                                  select(filter(test_ISR_err2,test_ISR_err2$area == "testdata_big"),4),
                                  groupref=test_ISR_err3$area)),
               "reference denominators must all be greater than zero", info="error test5")
  expect_error(data.frame(phe_isr(test_ISR_multiarea$count,
                                  test_ISR_multiarea$pop,
                                  test_ISR_refdata$refcount,
                                  select(filter(test_ISR_err3,test_ISR_err3$area == "testdata_big"),4),
                                  groupref=test_ISR_err3$area)),
               "reference denominators must all be greater than zero", info="error test6")
  expect_error(data.frame(phe_isr(test_ISR_multiarea$count,
                                  test_ISR_multiarea$pop,
                                  test_ISR_refdata$refcount,
                                  test_ISR_refdata$refpop,
                                  groupref=test_ISR_multiarea$area,
                                  conf.level = 0.8)),
               "confidence level must be between 90 and 100 or between 0.9 and 1", info="error test7")
  expect_error(data.frame(phe_isr(test_ISR_multiarea$count,
                                  test_ISR_multiarea$pop,
                                  test_ISR_refdata$refcount,
                                  test_ISR_refdata$refpop,
                                  groupref=test_ISR_multiarea$area,
                                  conf.level = 1.5)),
               "confidence level must be between 90 and 100 or between 0.9 and 1", info="error test8")
  expect_error(data.frame(phe_isr(select(filter(test_ISR_multiarea,test_ISR_multiarea$area == "testdata_big"),count),
                                  test_ISR_multiarea$pop,
                                  test_ISR_refdata$refcount,
                                  test_ISR_refdata$refpop,
                                  groupref=test_ISR_multiarea$area)),
               "numerator and denominator vectors must be of equal length", info="error test9")
  expect_error(data.frame(phe_isr(test_ISR_multiarea$count,
                                  test_ISR_multiarea$pop,
                                  test_ISR_refdata$refcount,
                                  select(filter(test_ISR_refdata,test_ISR_refdata$refcount > 3000),refpop),
                                  groupref=test_ISR_multiarea$area)),
               "reference numerator and reference denominator vectors must be of equal length", info="error test10")

# this check overwritten by R default error so could prob remove stop code in function ???
    expect_error(data.frame(phe_isr(test_ISR_multiarea$count,
                                  test_ISR_multiarea$pop,
                                  select(filter(test_ISR_refdata,test_ISR_refdata$refcount > 3000),refcount),
                                  select(filter(test_ISR_refdata,test_ISR_refdata$refcount > 3000),refpop),
                                  groupref=test_ISR_multiarea$area)),
               "numerator vector length must be a multiple of the reference numerator vector length", info="error test11")
})
