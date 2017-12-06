# load libraries
library(readxl)
library(dsrTest)

#set the working directory
setwd("C:/Users/Georgina.Anderson/Documents/R/Projects/PHEstatmethods")

# import test data
testdata_DSR_small     <- read_excel(".\\tests\\testthat\\testdata_DSR.xlsx", sheet="testdata_small",     col_names=TRUE)
testdata_DSR_big       <- read_excel(".\\tests\\testthat\\testdata_DSR.xlsx", sheet="testdata_big",       col_names=TRUE)
testdata_DSR_multiarea <- read_excel(".\\tests\\testthat\\testdata_DSR.xlsx", sheet="testdata_multiarea", col_names=TRUE)

# Run phe_dsr on testdata
small_results     <- phe_dsr(testdata_DSR_small$count,testdata_DSR_small$pop,testdata_DSR_small$stdpop, multiplier=10000)
big_results       <- phe_dsr(testdata_DSR_big$count,testdata_DSR_big$pop,testdata_DSR_big$stdpop, multiplier=10000)
multiarea_results <- phe_dsr(testdata_DSR_multiarea$count,testdata_DSR_multiarea$pop,multiplier=10000)





# ?? check out dsrTest to check whether Dobson method was used.
#dobsonControl()
#dsrTest()
