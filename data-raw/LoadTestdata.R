# This file loads all testdata used by the testthat scripts into R/sysdata
# there is also code to load all required libraries
library(dplyr)
library(testthat)
library(devtools)
library(binom)
library(readxl)

# use this code to save loaded data to sysdata folder:
#devtools::use_data(test_Prop_1, test_Prop_100,
#                     test_Rate_100, test_Rate_100000,
#                     test_Mean, test_Mean_results,
#                     test_DSR_multiarea, test_DSR_1976, test_DSR_err1, test_DSR_err2, test_DSR_err3, test_DSR_results,
#                     test_ISR_multiarea, test_ISR_refdata, test_ISR_err1, test_ISR_err2, test_ISR_err3, test_ISR_results,
#                     internal = TRUE, overwrite = TRUE)

# Proportions
test_Prop_1   <- read_excel(".\\tests\\testthat\\testdata_Proportion.xlsx", sheet="testdata_Prop_1",   col_names=TRUE)
test_Prop_100 <- read_excel(".\\tests\\testthat\\testdata_Proportion.xlsx", sheet="testdata_Prop_100", col_names=TRUE)

test_Prop_1$Area   <- as.factor(test_Prop_1$Area)
test_Prop_100$Area <- as.factor(test_Prop_100$Area)


#Rates
test_Rate_100000 <- read_excel(".\\tests\\testthat\\testdata_Rate.xlsx", sheet="testdata_Rate_100000", col_names=TRUE)
test_Rate_100    <- read_excel(".\\tests\\testthat\\testdata_Rate.xlsx", sheet="testdata_Rate_100",    col_names=TRUE)

test_Rate_100$Area      <- as.factor(test_Rate_100$Area)
test_Rate_100000$Area   <- as.factor(test_Rate_100000$Area)

#Means
test_Mean         <- read_excel(".\\tests\\testthat\\testdata_Mean.xlsx", sheet="testdata_Mean",         col_names=TRUE)
test_Mean_results <- read_excel(".\\tests\\testthat\\testdata_Mean.xlsx", sheet="testdata_Mean_results", col_names=TRUE)

test_Mean$area         <- as.factor(test_Mean$area)
test_Mean_results$area <- as.factor(test_Mean_results$area)


# DSRs
test_DSR_multiarea <- read_excel(".\\tests\\testthat\\testdata_DSR.xlsx", sheet="testdata_multiarea", col_names=TRUE)
test_DSR_1976      <- read_excel(".\\tests\\testthat\\testdata_DSR.xlsx", sheet="testdata_1976",      col_names=TRUE)
test_DSR_err1      <- read_excel(".\\tests\\testthat\\testdata_DSR.xlsx", sheet="testdata_err1",      col_names=TRUE)
test_DSR_err2      <- read_excel(".\\tests\\testthat\\testdata_DSR.xlsx", sheet="testdata_err2",      col_names=TRUE)
test_DSR_err3      <- read_excel(".\\tests\\testthat\\testdata_DSR.xlsx", sheet="testdata_err3",      col_names=TRUE)
test_DSR_results   <- read_excel(".\\tests\\testthat\\testdata_DSR.xlsx", sheet="testresults",        col_names=TRUE)

test_DSR_results$group <- as.factor(test_DSR_results$group)


# ISRs
test_ISR_multiarea <- read_excel(".\\tests\\testthat\\testdata_ISR.xlsx", sheet="testdata_multiarea", col_names=TRUE)
test_ISR_err1      <- read_excel(".\\tests\\testthat\\testdata_ISR.xlsx", sheet="testdata_err1",      col_names=TRUE)
test_ISR_err2      <- read_excel(".\\tests\\testthat\\testdata_ISR.xlsx", sheet="testdata_err2",      col_names=TRUE)
test_ISR_err3      <- read_excel(".\\tests\\testthat\\testdata_ISR.xlsx", sheet="testdata_err3",      col_names=TRUE)
test_ISR_results   <- read_excel(".\\tests\\testthat\\testdata_ISR.xlsx", sheet="testresults",        col_names=TRUE)
test_ISR_refdata   <- read_excel(".\\tests\\testthat\\testdata_ISR.xlsx", sheet="refdata",            col_names=TRUE)

test_ISR_results$group <- as.factor(test_ISR_results$group)
