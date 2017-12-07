library(testthat)
library(readxl)

# import test data
testdata_DSR_small     <- read_excel(".\\tests\\testthat\\testdata_DSR.xlsx", sheet="testdata_small",     col_names=TRUE)
testdata_DSR_big       <- read_excel(".\\tests\\testthat\\testdata_DSR.xlsx", sheet="testdata_big",       col_names=TRUE)
testdata_DSR_multiarea <- read_excel(".\\tests\\testthat\\testdata_DSR.xlsx", sheet="testdata_multiarea", col_names=TRUE)
testdata_results       <- read_excel(".\\tests\\testthat\\testdata_DSR.xlsx", sheet="testresults",   col_names=TRUE)

testdata_results$group <- as.factor(testdata_results$group)

#test calculations
test_that("dsrs and CIs calculate correctly",{
  expect_equal(phe_dsr(testdata_DSR_multiarea$count, testdata_DSR_multiarea$pop,
                       groupref=testdata_DSR_multiarea$area, multiplier=10000)[2:7],
               select(testdata_results,1:6), check.attributes=FALSE, check.names=FALSE)
})








# ?? check out dsrTest to check whether Dobson method was used.
#dobsonControl()
#dsrTest()
