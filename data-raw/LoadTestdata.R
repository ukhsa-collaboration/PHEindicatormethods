# This file loads all testdata used by the testthat scripts into R/sysdata
# there is also code to load all required libraries

library(dplyr)
library(testthat)
library(devtools)
library(readxl)

# esp2013
esp2013 <- c(5000,5500,5500,5500,6000,6000,6500,7000,7000,7000,7000,6500,6000,5500,5000,4000,2500,1500,1000)

# Byars Wilson
test_BW <- read_excel(".\\tests\\testthat\\testdata_Byars_Wilson.xlsx", sheet="testdata_B_W",   col_names=TRUE)

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
test_multigroup  <- read_excel(".\\tests\\testthat\\testdata_DSR_ISR_SMR.xlsx", sheet="testdata_multigroup", col_names=TRUE) %>%
  group_by(area,year)

test_ISR_results <- read_excel(".\\tests\\testthat\\testdata_DSR_ISR_SMR.xlsx", sheet="testresults_ISR", col_names=TRUE)
test_ISR_refdata <- read_excel(".\\tests\\testthat\\testdata_DSR_ISR_SMR.xlsx", sheet="refdata",         col_names=TRUE)
test_ISR_ownref  <- read_excel(".\\tests\\testthat\\testdata_DSR_ISR_SMR.xlsx", sheet="testdata_multiarea_ref", col_names=TRUE) %>%
  group_by(area)

# SII
SII_test_data <- read_excel("tests/testthat/testdata_SII.xlsx")

# Grouped SII test data
SII_test_grouped <- SII_test_data %>%
     group_by(Area, Grouping1, Grouping2)

# use this code to save loaded data to R\sysdata.rda file:
usethis::use_data(test_BW, test_Prop,
                    test_Rate,
                    test_Mean, test_Mean_Grp, test_Mean_results,
                    test_multiarea, test_multigroup, test_DSR_1976, test_err1, test_err2, test_err3, test_DSR_results,
                    test_ISR_refdata, test_ISR_results, test_ISR_ownref,
                    SII_test_data, SII_test_grouped,
                    internal = TRUE, overwrite = TRUE)

# SAVE EXTERNALLY AVAILABLE DATA IN data\XXXXXX.rda - data available to user
usethis::use_data(esp2013, LE_data, DSR_data, prevalence_data,
                  internal=FALSE, overwrite=TRUE)
