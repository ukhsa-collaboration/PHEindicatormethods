context("test_phe_funnels")


# test calculations
test_that("confidence limits calculate correctly for proportions",{
  funnel_table <- test_funnel_inputs %>%
    dplyr::select(numerator, denominator) %>%
    phe_funnels(numerator, denominator,
                type = "standard")
  expect_equal(data.frame(funnel_table),
               test_funnel_outputs,
               info = "Default funnel plot for proportions")
})

test_that("confidence limits calculate correctly with axis variation",{
  funnel_table <- test_funnel_inputs %>%
    dplyr::select(numerator, denominator) %>%
    filter(denominator < 31000) %>%
    phe_funnels(numerator, denominator)
  expect_equal(data.frame(funnel_table),
               test_funnel_outputs_axis_variation,
               info = "Default funnel plot for proportions with axis variation")
})

test_that("confidence limits calculate correctly for ratios (count)",{
  funnel_table <- test_funnel_ratio_inputs %>%
    phe_funnels(obs,
                expected,
                type = "full",
                statistic = "ratio",
                ratio_type = "count") %>%
    data.frame()
  test_funnel_ratio_counts <- test_funnel_ratio_outputs %>%
    mutate(across(ends_with("isr"),
                  function(x) NULL)) %>%
    rename_with(.fn = function(x) gsub("count", "limit", x),
                .cols = ends_with("count")) %>%
    mutate(statistic = "ratio (count)")
  expect_equal(funnel_table,
               test_funnel_ratio_counts,
               info = "Full funnel plot for ratios (count)")
})

test_that("confidence limits calculate correctly for ratios (count)",{
  funnel_table <- test_funnel_ratio_inputs %>%
    phe_funnels(obs,
                expected,
                type = "standard",
                statistic = "ratio",
                ratio_type = "isr") %>%
    data.frame()
  test_funnel_ratio_isrs <- test_funnel_ratio_outputs %>%
    mutate(across(ends_with("count"),
                  function(x) NULL)) %>%
    rename_with(.fn = function(x) gsub("isr", "limit", x),
                .cols = ends_with("isr"))
  expect_equal(funnel_table,
               test_funnel_ratio_isrs,
               info = "Default funnel plot for ratios (count)")
})

test_that("confidence limits calculate correctly for rates (dsr)",{
  funnel_table <- test_funnel_rate_funnels_input %>%
    phe_funnels(numerator = ev,
                rate = rate,
                multiplier = 1e5,
                statistic = "rate",
                rate_type = "dsr",
                years_of_data = 3)
  expect_equal(funnel_table,
               test_funnel_rate_funnels,
               info = "Funnel plot for rates (dsr)")
})

test_that("confidence limits calculate correctly for rates (dsr); with less than 10 events",{
  funnel_table <- test_funnel_rate_funnels_input %>%
    mutate(ev = case_when(ev == max(ev) ~ 5L,
                          TRUE ~ ev)) %>%
    phe_funnels(numerator = ev,
                rate = rate,
                multiplier = 1e5,
                statistic = "rate",
                rate_type = "crude",
                years_of_data = 3)
  expect_equal(funnel_table,
               test_funnel_rate_funnels_2,
               info = "Funnel plot for rates (dsr); with less than 10 events")
})

# test_that("confidence limits calculate correctly for rates (dsr); with less than 10 events and denominator supplied",{
#   funnel_table <- test_funnel_rate_funnels_input %>%
#     mutate(ev = case_when(ev == max(ev) ~ 0L,
#                           TRUE ~ ev)) %>%
#     phe_funnels(numerator = ev,
#                 rate = rate,
#                 denominator = pop,
#                 multiplier = 1e5,
#                 statistic = "rate",
#                 rate_type = "crude",
#                 years_of_data = 3)
#   expect_equal(funnel_table,
#                test_funnel_rate_funnels_2,
#                info = "Funnel plot for rates (dsr); with less than 10 events and denominator supplied")
# })


# test significance calculations
test_that("Significance for proportions calculates correctly", {
  expect_equal(data.frame(phe_funnel_significance(test_funnel_inputs[1:3], numerator, denominator)),
               test_funnel_inputs,
               info = "Funnel significance for proportions"
  )
})

test_that("Significance for ratios calculates correctly", {
  testing_ratio_sig <- test_funnel_ratio_inputs[1:2] %>%
    phe_funnel_significance(obs, expected,
                            statistic = "ratio") %>%
    data.frame()
  expect_equal(testing_ratio_sig,
               test_funnel_ratio_inputs,
               info = "Funnel significance for ratios"
  )
})

test_that("Significance for rates calculates correctly; dsr per 100,000 with 0", {
  testing_rate_sig <- test_funnel_rate_inputs[1:3] %>%
    phe_funnel_significance(numerator = count,
                            denominator = pop,
                            rate = rate_dsr,
                            statistic = "rate",
                            rate_type = "dsr",
                            multiplier = 1e5)
  expect_equal(testing_rate_sig,
               select(test_funnel_rate_inputs,
                      count, rate_dsr, pop,
                      significance = dsr_per_100000_with_0),
               info = "Funnel significance for rates; dsr per 100,000 with 0"
  )
})


test_that("Significance for rates calculates correctly; dsr per 100 with 0", {
  testing_rate_sig <- test_funnel_rate_inputs[1:3] %>%
    phe_funnel_significance(numerator = count,
                            denominator = pop,
                            rate = rate_dsr,
                            statistic = "rate",
                            rate_type = "dsr",
                            multiplier = 100)
  expect_equal(testing_rate_sig,
               select(test_funnel_rate_inputs,
                      count, rate_dsr, pop,
                      significance = dsr_per_100_with_0),
               info = "Funnel significance for rates; dsr per 100 with 0"
  )
})

test_that("Significance for rates calculates correctly; dsr per 100,000 without 0", {
  testing_rate_sig <- test_funnel_rate_inputs[1:3] %>%
    filter(count != 0) %>%
    phe_funnel_significance(numerator = count,
                            denominator = pop,
                            rate = rate_dsr,
                            statistic = "rate",
                            rate_type = "dsr",
                            multiplier = 1e5)
  expect_equal(testing_rate_sig,
               test_funnel_rate_inputs %>%
                 filter(count != 0) %>%
                 select(
                   count,
                   rate_dsr,
                   pop,
                   significance = dsr_per_100000_without_0),
               info = "Funnel significance for rates; dsr per 100,000 without 0"
  )
})


test_that("Significance for rates calculates correctly; dsr per 100 without 0", {
  testing_rate_sig <- test_funnel_rate_inputs[1:3] %>%
    filter(count != 0) %>%
    phe_funnel_significance(numerator = count,
                            denominator = pop,
                            rate = rate_dsr,
                            statistic = "rate",
                            rate_type = "dsr",
                            multiplier = 100)
  expect_equal(testing_rate_sig,
               test_funnel_rate_inputs %>%
                 filter(count != 0) %>%
                 select(
                   count,
                   rate_dsr,
                   pop,
                   significance = dsr_per_100_without_0),
               info = "Funnel significance for rates; dsr per 100 without 0"
  )
})

test_that("Significance for rates calculates correctly; crude per 100,000 with 0", {
  testing_rate_sig <- test_funnel_rate_inputs %>%
    select(count, rate_crude_per_100000, pop) %>%
    phe_funnel_significance(numerator = count,
                            denominator = pop,
                            rate = rate_crude_per_100000,
                            statistic = "rate",
                            rate_type = "crude",
                            multiplier = 1e5)
  expect_equal(testing_rate_sig,
               select(test_funnel_rate_inputs,
                      count, rate_crude_per_100000, pop,
                      significance = crude_per_100000_with_0),
               info = "Funnel significance for rates; crude per 100,000 with 0"
  )
})


test_that("Significance for rates calculates correctly; crude per 100 with 0", {
  testing_rate_sig <- test_funnel_rate_inputs %>%
    select(count, rate_crude_per_100, pop) %>%
    phe_funnel_significance(numerator = count,
                            denominator = pop,
                            rate = rate_crude_per_100,
                            statistic = "rate",
                            rate_type = "crude",
                            multiplier = 100)
  expect_equal(testing_rate_sig,
               select(test_funnel_rate_inputs,
                      count, rate_crude_per_100, pop,
                      significance = crude_per_100_with_0),
               info = "Funnel significance for rates; crude per 100 with 0"
  )
})

test_that("Significance for rates calculates correctly; crude per 100,000 without 0", {
  testing_rate_sig <- test_funnel_rate_inputs %>%
    filter(count != 0) %>%
    select(count, rate_crude_per_100000, pop) %>%
    phe_funnel_significance(numerator = count,
                            denominator = pop,
                            rate = rate_crude_per_100000,
                            statistic = "rate",
                            rate_type = "crude",
                            multiplier = 1e5)
  expect_equal(testing_rate_sig,
               test_funnel_rate_inputs %>%
                 filter(count != 0) %>%
                 select(
                   count,
                   rate_crude_per_100000,
                   pop,
                   significance = crude_per_100000_without_0),
               info = "Funnel significance for rates; crude per 100,000 without 0"
  )
})


test_that("Significance for rates calculates correctly; crude per 100 without 0", {
  testing_rate_sig <- test_funnel_rate_inputs %>%
    filter(count != 0) %>%
    select(count, rate_crude_per_100, pop) %>%
    phe_funnel_significance(numerator = count,
                            denominator = pop,
                            rate = rate_crude_per_100,
                            statistic = "rate",
                            rate_type = "crude",
                            multiplier = 100)
  expect_equal(testing_rate_sig,
               test_funnel_rate_inputs %>%
                 filter(count != 0) %>%
                 select(
                   count,
                   rate_crude_per_100,
                   pop,
                   significance = crude_per_100_without_0),
               info = "Funnel significance for rates; crude per 100 without 0"
  )
})


# funnel point conversion function works ----------------------------------

test_that("phe_funnel_convert_points works for dsrs with events less than 5", {
  function_output <- test_funnel_rate_funnels_input %>%
    select(ev, rate) %>%
    mutate(ev = case_when(
      ev == max(ev) ~ 5L,
      TRUE ~ ev
    )) %>%
    phe_funnel_convert_points(
      numerator = ev,
      rate = rate,
      rate_type = "dsr",
      years_of_data = 3,
      multiplier = 1e5)
  expect_equal(
    function_output,
    test_funnel_rate_funnels_input %>%
      mutate(ev = case_when(
        ev == max(ev) ~ 5L,
        TRUE ~ ev
      )) %>%
      select(ev, rate,
             rate_chart,
             denominator_derived),
    info = "phe_funnel_convert_points works for dsrs with events less than 5"
  )
})

test_that("phe_funnel_convert_points works for crude with events less than 5", {
  function_output <- test_funnel_rate_funnels_input %>%
    select(ev, rate) %>%
    phe_funnel_convert_points(
      numerator = ev,
      rate = rate,
      rate_type = "crude",
      years_of_data = 3,
      multiplier = 1e5)
  expect_equal(
    function_output,
    test_funnel_rate_funnels_input %>%
      select(ev, rate,
             rate_chart = rate_chart_crude,
             denominator_derived = denominator_derived_crude),
    info = "phe_funnel_convert_points works for dsrs with events less than 5"
  )
})

test_that("phe_funnel_convert_points works for dsrs with 0 event record and denominators supplied", {
  function_output <- test_funnel_rate_funnels_input %>%
    select(ev, rate, pop) %>%
    mutate(ev = case_when(
      ev == max(ev) ~ 0L,
      TRUE ~ ev
    )) %>%
    phe_funnel_convert_points(
      numerator = ev,
      rate = rate,
      denominator = pop,
      rate_type = "crude",
      years_of_data = 5,
      multiplier = 1e5)
  expect_equal(
    function_output,
    test_funnel_rate_funnels_input %>%
      mutate(ev = case_when(
        ev == max(ev) ~ 0L,
        TRUE ~ ev
      )) %>%
      select(ev, rate, pop,
             rate_chart = rate_chart_crude_with_denom,
             denominator_derived = denominator_derived_crude_with_denom),
    info = "phe_funnel_convert_points works for dsrs with 0 event record and denominators supplied"
  )
})


# test error handling
test_that("incorrect statistic argument", {
  expect_error(
    test_funnel_inputs %>%
      dplyr::select(numerator, denominator) %>%
      filter(denominator < 31000) %>%
      phe_funnels(numerator, denominator,
                  statistic = "pop"),
    "'arg' should be one of \"proportion\", \"ratio\"",
    info = "incorrect argument specified to statistic for phe_funnels"
  )
})

test_that("incorrect type argument", {
  expect_error(
    test_funnel_inputs %>%
      dplyr::select(numerator, denominator) %>%
      filter(denominator < 31000) %>%
      phe_funnels(numerator, denominator,
                  type = "srtd"),
    "'arg' should be one of \"full\", \"standard\"",
    info = "incorrect argument specified to type for phe_funnels"
  )
})

test_that("denominators must be greater than zero", {
  expect_error(
    phe_funnel_significance(
      data.frame(
        area = c("Area1", "Area2", "Area3"),
        obs = c(65, 80, 30),
        pop = c(100, 100, 0)
      ),
      numerator = obs,
      denominator = pop
    ),
    "denominators must be greater than zero",
    info = "check your denominators so they are greater than zero"
  )
})

test_that("numerators must be greater than or equal to zero", {
  expect_error(
    phe_funnel_significance(
      data.frame(
        area = c("Area1", "Area2", "Area3"),
        obs = c(65, -80, 30),
        pop = c(100, 100, 200)
      ),
      numerator = obs,
      denominator = pop
    ),
    "numerators must be greater than or equal to zero",
    info = "error num < 0"
  )
})

test_that("numerators must be less than or equal to denominator for a proportion statistic", {
  expect_error(
    phe_funnel_significance(
      data.frame(
        area = c("Area1", "Area2", "Area3"),
        obs = c(65, 80, 30),
        pop = c(100, 100, 20)
      ),
      numerator = obs,
      denominator = pop
    ),
    "numerators must be less than or equal to denominator for a proportion statistic",
    info = "error numerator > denominator"
  )
})


test_that("phe_funnels requires data, numerator and denominator", {
  expect_error(
    phe_funnels(
      data.frame(
        area = c("Area1", "Area2", "Area3"),
        pop = c(100, 100, 20)
      ), pop),
    "at least 3 arguments are required for ratios and proportions: data, numerator, denominator",
    info = "check the parameters passed into the function"
  )
})

test_that("phe_funnel_significance input error for rate; data, numerator and rate", {
  expect_error(
    phe_funnel_significance(
      data.frame(
        area = c("Area1", "Area2", "Area3"),
        num = c(100, 100, 20)
      ),
      num, statistic = "rate",
      rate_type = "dsr",
      multiplier = 100),
    "at least 3 arguments are required for rates: data, numerator, rate",
    info = "phe_funnel_significance input error for rate; data, numerator and rate"
  )
})

test_that("phe_funnel_significance input error for rate; denominator field required when 0 in numerator", {
  expect_error(
    phe_funnel_significance(
      data.frame(
        area = c("Area1", "Area2", "Area3"),
        num = c(0, 100, 20),
        rate = c(0, 142, 111)
      ),
      numerator = num,
      rate = rate,
      statistic = "rate",
      rate_type = "crude",
      multiplier = 100),
    "for rates, where there are 0 events for a record, the denominator field needs to be provided using the denominator argument",
    info = "phe_funnel_significance input error for rate; denominator field required when 0 in numerator"
  )
})

test_that("phe_funnel_significance input error for rate; missing rate multiplier", {
  expect_error(
    phe_funnel_significance(
      data.frame(
        area = c("Area1", "Area2", "Area3"),
        num = c(50, 100, 20),
        rate = c(100, 142, 111)
      ),
      numerator = num,
      rate = rate,
      statistic = "rate",
      rate_type = "crude"),
    "for rates, a multiplier is required to test the significance of the points",
    info = "phe_funnel_significance input error for rate; missing rate multiplier"
  )
})

test_that("phe_funnel_convert_points input error; missing 3 arguments", {
  expect_error(
    test_funnel_rate_funnels_input %>%
      phe_funnel_convert_points(
        numerator = ev,
        rate_type = "dsr",
        years_of_data = 3,
        multiplier = 1e5),
    "at least 3 arguments are required for rates: data, numerator, rate",
    info = "phe_funnel_convert_points input error; missing 3 arguments"
  )
})

test_that("phe_funnel_convert_points input error; missing multiplier", {
  expect_error(
    test_funnel_rate_funnels_input %>%
     phe_funnel_convert_points(
        numerator = ev,
        rate = rate,
        rate_type = "dsr",
        years_of_data = 3),
    "a multiplier is required to convert the input data",
    info = "phe_funnel_convert_points input error; missing multiplier"
  )
})


