# This file loads all testdata used by the testthat scripts into R/sysdata
# there is also code to load all required libraries

#library(dplyr)
#library(testthat)
#library(devtools)
#library(readxl)

# use this code to save loaded data to sysdata folder:
#devtools::use_data(test_Prop,
#                     test_Rate,
#                     test_Mean, test_Mean_Grp, test_Mean_results,
#                     test_multiarea, test_DSR_1976, test_err1, test_err2, test_err3, test_DSR_results,
#                     test_ISR_refdata, test_ISR_results,
#                     internal = TRUE, overwrite = TRUE)

# esp2013
#esp2013 <- c(5000,5500,5500,5500,6000,6000,6500,7000,7000,7000,7000,6500,6000,5500,5000,4000,2500,1500,1000)

# Proportions
test_Prop   <- read_excel(".\\tests\\testthat\\testdata_Proportion.xlsx", sheet="testdata_Prop",   col_names=TRUE)

#Rates
test_Rate <- read_excel(".\\tests\\testthat\\testdata_Rate.xlsx", sheet="testdata_Rate", col_names=TRUE)

#Means
test_Mean         <- read_excel(".\\tests\\testthat\\testdata_Mean.xlsx", sheet="testdata_Mean",         col_names=TRUE)
test_Mean_results <- read_excel(".\\tests\\testthat\\testdata_Mean.xlsx", sheet="testdata_Mean_results", col_names=TRUE)

test_Mean_Grp <- group_by(test_Mean,area)


# DSRs, ISRs and SMRs
test_multiarea   <- read_excel(".\\tests\\testthat\\testdata_DSR_ISR_SMR.xlsx", sheet="testdata_multiarea", col_names=TRUE) %>%
                       group_by(area)
test_DSR_1976    <- read_excel(".\\tests\\testthat\\testdata_DSR_ISR_SMR.xlsx", sheet="testdata_1976",   col_names=TRUE)
test_err1        <- read_excel(".\\tests\\testthat\\testdata_DSR_ISR_SMR.xlsx", sheet="testdata_err1",   col_names=TRUE)
test_err2        <- read_excel(".\\tests\\testthat\\testdata_DSR_ISR_SMR.xlsx", sheet="testdata_err2",   col_names=TRUE) %>%
                       group_by(area)
test_err3        <- read_excel(".\\tests\\testthat\\testdata_DSR_ISR_SMR.xlsx", sheet="testdata_err3",   col_names=TRUE)
test_DSR_results <- read_excel(".\\tests\\testthat\\testdata_DSR_ISR_SMR.xlsx", sheet="testresults_DSR", col_names=TRUE)

test_ISR_results <- read_excel(".\\tests\\testthat\\testdata_DSR_ISR_SMR.xlsx", sheet="testresults_ISR", col_names=TRUE)
test_ISR_refdata <- read_excel(".\\tests\\testthat\\testdata_DSR_ISR_SMR.xlsx", sheet="refdata",         col_names=TRUE)
test_ISR_ownref  <- read_excel(".\\tests\\testthat\\testdata_DSR_ISR_SMR.xlsx", sheet="testdata_multiarea_ref", col_names=TRUE) %>%
                      group_by(area)
