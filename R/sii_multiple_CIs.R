# Test adding multiple confidence interval functionnality to phe_sii
library(PHEindicatormethods)
library(tidyverse)
library(readxl)

source("R/utils.R")

# load and group test data
SII_test_data <- read_excel("tests/testthat/testdata_SII.xlsx") %>%
                     group_by(Area, Grouping1, Grouping2)

# take subset of rows for testing
SII_test_data <- SII_test_data[21:40, 3:13]


# run phe_sii() normally
normal_result_95 <- phe_sii(SII_test_data,
                  Quantile, Population,
                  value_type = 0,
                  value = Value,
                  lower_cl = LowerCL,
                  upper_cl = UpperCL,
                  confidence = 0.95,
                  repetitions = 100000,
                  rii = FALSE,
                  type = "standard")

normal_result_99_8 <- phe_sii(SII_test_data,
                            Quantile, Population,
                            value_type = 0,
                            value = Value,
                            lower_cl = LowerCL,
                            upper_cl = UpperCL,
                            confidence = 0.998,
                            repetitions = 100000,
                            rii = FALSE,
                            type = "standard")

# test the new multiple CI functionality
debug(phe_sii_test)
undebug(phe_sii_test)
test_result <- phe_sii_test(SII_test_data,
                         Quantile, Population,
                         value_type = 0,
                         value = Value,
                         lower_cl = LowerCL,
                         upper_cl = UpperCL,
                         confidence = c(0.95, 0.998),
                         repetitions = 100000,
                         rii = FALSE,
                         type = "standard")


# PART 1 - Test returning multiple CIs on the first subgroup of data

confidence1 <- 0.95
confidence2 <- c(0.95, 0.98)

debug(SimulationFunc)
undebug(SimulationFunc)

lower_cls_1 <- confidence1 %>%
    lapply(SimulationFunc,
           data = (test_result$data)[[1]],
           value = value,
           value_type = 1,
           se = se_calc,
           repeats = 100,
           sqrt_a = sqrt_a,
           b_sqrt_a = b_sqrt_a,
           rii = FALSE,
           reliability_stat = FALSE)

lower_cls_2 <- confidence2 %>%
  lapply(SimulationFunc,
         data = (test_result$data)[[1]],
         value = value,
         value_type = 1,
         se = se_calc,
         repeats = 100,
         sqrt_a = sqrt_a,
         b_sqrt_a = b_sqrt_a,
         rii = FALSE,
         reliability_stat = FALSE)



# PART 2 - Extend to run over all data subgroups

debug(SimulationFunc)
undebug(SimulationFunc)

lower_cls_2 <- test_result$data %>%
  map2(confidence2,
         SimulationFunc,
         value = value,
         value_type = 0,
         se = se_calc,
         repeats = 100,
         sqrt_a = sqrt_a,
         b_sqrt_a = b_sqrt_a,
         rii = FALSE,
         reliability_stat = FALSE)




my_fun <- function(data, confidence) {

  map(data, ~ SimulationFunc(data = .,
                                  value = value,
                                  value_type = 1,
                                  se = se_calc,
                                  repeats = 100,
                                  sqrt_a = sqrt_a,
                                  b_sqrt_a = b_sqrt_a,
                                  rii = FALSE,
                                  reliability_stat = FALSE))

}

lower_cls_1 <- confidence1 %>%
  lapply(my_fun, data = test$data)

lower_cls_2 <- confidence2 %>%
  lapply(my_fun, data = test$data)




