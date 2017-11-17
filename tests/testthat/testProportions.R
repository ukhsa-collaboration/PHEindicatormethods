
#Test binomial function ---------------------------------------------------------------------------
library(testthat)
library(PHEstatmethods)

# context ?? what does this line do?  do I need it?

test_that("proportion and CIs calculate correctly",{
  expect_equal(phe_proportion(35078,41980),c(35078,41980,0.835588375,0.83201210,0.83910324))
  expect_equal(phe_proportion(35078,41980,conf.level=0.998),c(35078,41980,0.835588375,0.82992190,0.84110220))
  expect_equal(phe_proportion(65,100,conf.level=0.95),c(65,100,0.65,0.55254443,0.73635752))
  expect_equal(phe_proportion(65,100,conf.level=0.998),c(65,100,0.65,0.49549466,0.77835406))
})

test_that("error generated when numerator is less than zero",{
  expect_equal(phe_proportion(-65,100),"numerator must be greater than or equal to zero")
})

test_that("error generated when denominator is less than or equal to zero",{
  expect_equal(phe_proportion(65,-100),"denominator must be greater than zero")
  expect_equal(phe_proportion(65,0),"denominator must be greater than zero")
})

test_that("error generated when numerator is greater than denominator",{
  expect_equal(phe_proportion(100,65),"numerator must be less than or equal to denominator for a proportion statistic")
})

test_that("error generated when max is not equal to 1 or 100",{
  expect_equal(phe_proportion(65,100,max=1000),"function phe.binom can only output a proportion (max=1) or a percentage (max=100)")
})



_________________________________________________________________________________________
# import test data
prop_test_data <- read_excel(".\\tests\\testthat\\Test Data.xlsx", sheet=1, col_names=TRUE)

# test function
output_95 <- binom.wilson.ci(PropTestData$numerator,PropTestData$denominator) %>%
  select(x, n, mean, lower, upper)
output_998 <- binom.wilson.ci(PropTestData$numerator,PropTestData$denominator, conf.level=0.998) %>%
  select(x, n, mean, lower, upper)

# compare output with verified source
compare <- full_join(PropTestData, output_95, by = c("numerator"="x","denominator"="n")) %>%
  full_join(output_998, by = c("numerator"="x","denominator"="n"))

# output any discrepancies
discreps <- subset(compare, proportion != mean.x) %>%
  bind_rows(subset(compare, proportion != mean.y)) %>%
  bind_rows(subset(compare, round(lower95CI,8) != round(lower.x,8))) %>%
  bind_rows(subset(compare, round(upper95CI,8) != round(upper.x,8))) %>%
  bind_rows(subset(compare, round(lower998CI,8) != round(lower.y,8))) %>%
  bind_rows(subset(compare, round(upper998CI,8) != round(upper.y,8)))

discreps

