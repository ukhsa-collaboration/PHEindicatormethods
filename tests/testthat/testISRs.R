library(testthat)
library(readxl)

# Load test data
bigdata          <- read_excel(".\\tests\\testthat\\testdata_ISR.xlsx", sheet="testdata_big", col_names=TRUE)
smalldata        <- read_excel(".\\tests\\testthat\\testdata_ISR.xlsx", sheet="testdata_small", col_names=TRUE)
multiareadata    <- read_excel(".\\tests\\testthat\\testdata_ISR.xlsx", sheet="testdata_multiarea", col_names=TRUE)
testdata_results <- read_excel(".\\tests\\testthat\\testdata_ISR.xlsx", sheet="testresults", col_names=TRUE)
refdata          <- read_excel(".\\tests\\testthat\\testdata_ISR.xlsx", sheet="refdata", col_names=TRUE)

testdata_results$group <- as.factor(testdata_results$group)

#test calculations
test_that("isrs and CIs calculate correctly",{
  expect_equal(phe_isr(multiareadata$count, multiareadata$pop,
                       refdata$refcount, refdata$refpop,
                       groupref=multiareadata$area)[2:7],
               select(testdata_results,1:6), check.attributes=FALSE, check.names=FALSE)
  expect_equal(phe_isr(multiareadata$count, multiareadata$pop,
                       refdata$refcount, refdata$refpop,
                       groupref=multiareadata$area, type="rate",multiplier=100000)[2:7],
               select(testdata_results,1:6), check.attributes=FALSE, check.names=FALSE)
})

