# load libraries
library(readxl)
library(dsrTest)

#set the working directory
setwd("C:/Users/Georgina.Anderson/Documents/R/Projects/PHEstatmethods")

# import test data
DSR_testdata_small <- read_excel(".\\tests\\testthat\\DSR_testdata.xlsx", sheet=1, col_names=TRUE)
DSR_testdata_big <- read_excel(".\\tests\\testthat\\DSR_testdata.xlsx", sheet=3, col_names=TRUE)


# Run phe_dsr on testdata
small_results <- phe_dsr(DSR_testdata_small$count,DSR_testdata_small$pop,DSR_testdata_small$stdpop, multiplier=10000)
big_results <- phe_dsr(DSR_testdata_big$count,DSR_testdata_big$pop,DSR_testdata_big$stdpop, multiplier=10000)


# ?? check out dsrTest to check whether Dobson method was used.
#dobsonControl()
#dsrTest()
