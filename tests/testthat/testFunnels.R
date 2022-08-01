
# test calculate_funnel_limits calculations
test_that("confidence limits calculate correctly for proportions",{
  funnel_table <- test_funnel_inputs %>%
    dplyr::select(numerator, denominator) %>%
    calculate_funnel_limits(numerator, denominator,
                            statistic = "proportion",
                            multiplier = 100,
                type = "standard")
  expect_equal(data.frame(funnel_table),
               test_funnel_outputs,
               info = "Default funnel plot for proportions")
})

test_that("confidence limits calculate correctly with axis variation",{
  funnel_table <- test_funnel_inputs %>%
    dplyr::select(numerator, denominator) %>%
    filter(denominator < 31000) %>%
    calculate_funnel_limits(numerator, denominator,
                            statistic = "proportion",
                            multiplier = 100)
  expect_equal(data.frame(funnel_table),
               test_funnel_outputs_axis_variation,
               info = "Default funnel plot for proportions with axis variation")
})

test_that("confidence limits calculate correctly for ratios (count)",{
  funnel_table <- test_funnel_ratio_inputs %>%
    calculate_funnel_limits(obs,
                expected,
                type = "full",
                statistic = "ratio",
                multiplier = 100,
                ratio_type = "count") %>%
    data.frame()
  test_funnel_ratio_counts <- test_funnel_ratio_outputs %>%
    mutate(across(ends_with("isr"),
                  function(x) NULL)) %>%
    rename_with(.fn = function(x) gsub("count", "limit", x),
                .cols = ends_with("count")) %>%
    mutate(statistic = "ratio (count)",
           method = "Poisson")
  expect_equal(funnel_table,
               test_funnel_ratio_counts,
               info = "Full funnel plot for ratios (count)")
})

test_that("confidence limits calculate correctly for ratios (isr)",{
  funnel_table <- test_funnel_ratio_inputs %>%
    calculate_funnel_limits(obs,
                expected,
                type = "standard",
                statistic = "ratio",
                multiplier = 100,
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
    calculate_funnel_limits(numerator = ev,
                rate = rate,
                multiplier = 1e5,
                statistic = "rate",
                rate_type = "dsr",
                years_of_data = 3)
  expect_equal(funnel_table,
               test_funnel_rate_funnels,
               info = "Funnel plot for rates (dsr)")
})

test_that("confidence limits calculate correctly for rates (crude); with less than 10 events",{
  funnel_table <- test_funnel_rate_funnels_input %>%
    mutate(ev = case_when(ev == max(ev) ~ 5L,
                          TRUE ~ ev)) %>%
    calculate_funnel_limits(numerator = ev,
                rate = rate,
                multiplier = 1e5,
                statistic = "rate",
                rate_type = "crude",
                years_of_data = 3)
  expect_equal(funnel_table,
               test_funnel_rate_funnels_2,
               info = "Funnel plot for rates (crude); with less than 10 events")
})

test_that("confidence limits calculate correctly for rates (crude); with 0 events and denominator supplied",{
  funnel_table <- test_funnel_rate_funnels_input %>%
    dplyr::select(ev, rate) %>%
    mutate(pop = 1e5 * ev / rate,
           rate = case_when(
             ev == max(ev) ~ 0L,
             TRUE ~ rate
           ),
           ev = case_when(
             ev == max(ev) ~ 0L,
             TRUE ~ ev
           )) %>%
    filter(pop > max(pop) / 2) %>%
    calculate_funnel_limits(numerator = ev,
                rate = rate,
                denominator = pop,
                multiplier = 1e5,
                statistic = "rate",
                rate_type = "crude",
                years_of_data = 3)
  expect_equal(funnel_table,
               test_funnel_rate_funnels_3,
               info = "Funnel plot for rates (crude); with 0 events and denominator supplied")
})


# test assign_funnel_significance calculations
test_that("Significance for proportions calculates correctly", {
  expect_equal(data.frame(assign_funnel_significance(
    test_funnel_inputs[1:3], numerator, denominator, statistic = "proportion")),
               test_funnel_inputs,
               info = "Funnel significance for proportions"
  )
})

test_that("Significance for ratios calculates correctly", {
  testing_ratio_sig <- test_funnel_ratio_inputs[1:2] %>%
    assign_funnel_significance(obs, expected,
                            statistic = "ratio") %>%
    data.frame()
  expect_equal(testing_ratio_sig,
               test_funnel_ratio_inputs,
               info = "Funnel significance for ratios"
  )
})

test_that("Significance for rates calculates correctly; dsr per 100,000 with 0", {
  testing_rate_sig <- test_funnel_rate_inputs[1:3] %>%
    assign_funnel_significance(numerator = count,
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


test_that("Significance for rates calculates correctly; crude rate per 100 with 0", {
  testing_rate_sig <- select(test_funnel_rate_inputs, count, rate_crude_per_100, pop) %>%
    assign_funnel_significance(numerator = count,
                            denominator = pop,
                            rate = rate_crude_per_100,
                            statistic = "rate",
                            rate_type = "crude",
                            multiplier = 100)
  expect_equal(testing_rate_sig,
               select(test_funnel_rate_inputs,
                      count, rate_crude_per_100, pop,
                      significance = crude_per_100_with_0),
               info = "Funnel significance for rates; dsr per 100 with 0"
  )
})

test_that("Significance for rates calculates correctly; dsr per 100,000 without 0", {
  testing_rate_sig <- test_funnel_rate_inputs[1:3] %>%
    filter(count != 0) %>%
    assign_funnel_significance(numerator = count,
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
    assign_funnel_significance(numerator = count,
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
    assign_funnel_significance(numerator = count,
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
    assign_funnel_significance(numerator = count,
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
    assign_funnel_significance(numerator = count,
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
    assign_funnel_significance(numerator = count,
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


# test calculate_funnel_points works ----------------------------------

test_that("calculate_funnel_points works for dsrs with events less than 5", {
  function_output <- test_funnel_rate_funnels_input %>%
    select(ev, rate) %>%
    mutate(ev = case_when(
      ev == max(ev) ~ 5L,
      TRUE ~ ev
    )) %>%
    calculate_funnel_points(
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
    info = "calculate_funnel_points works for dsrs with events less than 5"
  )
})

test_that("calculate_funnel_points works for crude with events less than 5", {
  function_output <- test_funnel_rate_funnels_input %>%
    select(ev, rate) %>%
    calculate_funnel_points(
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
    info = "calculate_funnel_points works for dsrs with events less than 5"
  )
})

test_that("calculate_funnel_points works for dsrs with 0 event record and denominators supplied", {
  function_output <- test_funnel_rate_funnels_input %>%
    select(ev, rate, pop) %>%
    mutate(ev = case_when(
      ev == max(ev) ~ 0L,
      TRUE ~ ev
    )) %>%
    calculate_funnel_points(
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
    info = "calculate_funnel_points works for dsrs with 0 event record and denominators supplied"
  )
})


# test error handling -----------------------------------------------------
test_that("incorrect statistic argument", {
  skip_on_covr()
  expect_error(
    test_funnel_inputs %>%
      dplyr::select(numerator, denominator) %>%
      filter(denominator < 31000) %>%
      calculate_funnel_limits(numerator, denominator,
                              multiplier = 100,
                              statistic = "pop"),
    "should be one of",
    info = "incorrect argument specified to statistic for calculate_funnel_limits"
  )
})

test_that("incorrect type argument", {
  skip_on_covr()
  expect_error(
    test_funnel_inputs %>%
      dplyr::select(numerator, denominator) %>%
      filter(denominator < 31000) %>%
      calculate_funnel_limits(numerator, denominator,
                              statistic = "proportion",
                              multiplier = 100,
                              type = "srtd"),
    "should be one of",
    info = "incorrect argument specified to type for calculate_funnel_limits"
  )
})

test_that("denominators must be greater than zero", {
  expect_error(
    assign_funnel_significance(
      data.frame(
        area = c("Area1", "Area2", "Area3"),
        obs = c(65, 80, 30),
        pop = c(100, 100, 0)
      ),
      numerator = obs,
      denominator = pop,
      statistic = "proportion",
      multiplier = 100
    ),
    "denominators must be greater than zero",
    info = "check your denominators so they are greater than zero"
  )
})

test_that("numerators must be greater than or equal to zero", {
  expect_error(
    assign_funnel_significance(
      data.frame(
        area = c("Area1", "Area2", "Area3"),
        obs = c(65, -80, 30),
        pop = c(100, 100, 200)
      ),
      numerator = obs,
      denominator = pop,
      statistic = "proportion",
      multiplier = 100
    ),
    "numerators must be greater than or equal to zero",
    info = "error num < 0"
  )
})

test_that("numerators must be less than or equal to denominator for a proportion statistic", {
  expect_error(
    assign_funnel_significance(
      data.frame(
        area = c("Area1", "Area2", "Area3"),
        obs = c(65, 80, 30),
        pop = c(100, 100, 20)
      ),
      numerator = obs,
      denominator = pop,
      statistic = "proportion",
      multiplier = 100,
    ),
    "numerators must be less than or equal to denominator for a proportion statistic",
    info = "error numerator > denominator"
  )
})

test_that("testing required arguments are provided to assign_funnel_significance for a proportion statistic", {
  expect_error(
    assign_funnel_significance(
      data.frame(
        area = c("Area1", "Area2", "Area3"),
        obs = c(65, 80, 30),
        pop = c(100, 100, 20)
      ),
      numerator = obs,
      statistic = "proportion",
      multiplier = 100
    ),
    "the following arguments are required for ratios and proportions: data, numerator, denominator",
    info = "testing required arguments are provided in assign_funnel_significance for proportion"
  )
})


test_that("calculate_funnel_limits requires statistic to be passed", {
  expect_error(
    calculate_funnel_limits(
      data.frame(
        area = c("Area1", "Area2", "Area3"),
        pop = c(100, 100, 20)
      ),
      numerator,
      denominator,
      multiplier = 100),
    "statistic must be provided as proportion, rate or ratio",
    info = "check the parameters passed into the function"
  )
})

test_that("calculate_funnel_limits requires data, numerator, denominator and multiplier for proportions", {
  expect_error(
    calculate_funnel_limits(
      data.frame(
        area = c("Area1", "Area2", "Area3"),
        pop = c(100, 100, 20)
      ),
      pop,
      statistic = "proportion",
      multiplier = 100),
    "the following arguments are required for proportions: data, numerator, denominator, multiplier",
    info = "check the parameters passed into the function"
  )
})

test_that("calculate_funnel_limits requires data, numerator, denominator and multiplier for proportions", {
  expect_error(
    calculate_funnel_limits(
      data.frame(
        area = c("Area1", "Area2", "Area3"),
        pop = c(100, 100, 20)
      ),
      pop,
      statistic = "ratio",
      multiplier = 100),
    "the following arguments are required for ratios: data, numerator, denominator, ratio_type, multiplier",
    info = "check the parameters passed into the function"
  )
})

test_that("calculate_funnel_limits requires data, numerator and denominator for rates", {
  expect_error(
    calculate_funnel_limits(
      data = data.frame(
        area = c("Area1", "Area2", "Area3"),
        num = c(100, 100, 20)
      ),
      numerator = num,
      statistic = "rate"),
    paste0("the following arguments are required for rates: ",
           "data, numerator, rate, rate_type, multiplier, years_of_data"),
    info = "check the parameters passed into the function for rates"
  )
})

test_that("calculate_funnel_limits requires denominator where numerator has 0 value for rates", {
  expect_error(
    calculate_funnel_limits(
      data = data.frame(
        area = c("Area1", "Area2", "Area3"),
        num = c(100, 100, 0),
        rate = c(50, 25, 0)
      ),
      numerator = num,
      rate = rate,
      rate_type = "dsr",
      statistic = "rate",
      multiplier = 100,
      years_of_data = 1),
    "for rates, where there are 0 events for a record, the denominator field needs to be provided using the denominator argument",
    info = "check the denominator field available when required for rates"
  )
})

test_that("calculate_funnel_limits required years_of_data when calculating rates", {
  expect_error(
    calculate_funnel_limits(
      data = tibble(
        area = c("Area1", "Area2", "Area3"),
        num = c(100, 100, 50),
        rate = c(50, 25, 15),
        pop = 1e3 * num / rate
      ),
      numerator = num,
      rate = rate,
      rate_type = "crude",
      denominator = pop,
      statistic = "rate",
      multiplier = 100),
    paste0("the following arguments are required for rates: ",
           "data, numerator, rate, rate_type, multiplier, years_of_data"),
    info = "check years_of_data is provided for rates"
  )
})

test_that("assign_funnel_significance input error for statistic", {
  expect_error(
    assign_funnel_significance(
      data.frame(
        area = c("Area1", "Area2", "Area3"),
        num = c(100, 100, 20)
      ),
      num,
      rate_type = "dsr",
      multiplier = 100),
    "statistic must be provided as proportion, rate or ratio",
    info = "assign_funnel_significance input error for rate; data, numerator and rate"
  )
})

test_that("assign_funnel_significance input error for rate; data, numerator and rate", {
  expect_error(
    assign_funnel_significance(
      data.frame(
        area = c("Area1", "Area2", "Area3"),
        num = c(100, 100, 20)
      ),
      num,
      statistic = "rate",
      rate_type = "dsr",
      multiplier = 100),
    paste0("the following arguments are required for rates: ",
           "data, numerator, rate, rate_type, multiplier"),
    info = "assign_funnel_significance input error for rate; data, numerator and rate"
  )
})

test_that("assign_funnel_significance input error for rate; denominator field required when 0 in numerator", {
  expect_error(
    assign_funnel_significance(
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
    info = "assign_funnel_significance input error for rate; denominator field required when 0 in numerator"
  )
})

test_that("assign_funnel_significance input error for rate; missing rate multiplier", {
  expect_error(
    assign_funnel_significance(
      data.frame(
        area = c("Area1", "Area2", "Area3"),
        num = c(50, 100, 20),
        rate = c(100, 142, 111)
      ),
      numerator = num,
      rate = rate,
      statistic = "rate",
      rate_type = "crude"),
    paste0("the following arguments are required for rates: ",
           "data, numerator, rate, rate_type, multiplier"),
    info = "assign_funnel_significance input error for rate; missing rate multiplier"
  )
})

test_that("calculate_funnel_points input error; missing 3 arguments", {
  expect_error(
    test_funnel_rate_funnels_input %>%
      calculate_funnel_points(
        numerator = ev,
        rate_type = "dsr",
        years_of_data = 3,
        multiplier = 1e5),
    paste0("the following arguments are required for rates: ",
           "data, numerator, rate, rate_type, years_of_data, multiplier"),
    info = "calculate_funnel_points input error; missing 3 arguments"
  )
})

test_that("calculate_funnel_points input error; missing multiplier", {
  expect_error(
    test_funnel_rate_funnels_input %>%
     calculate_funnel_points(
        numerator = ev,
        rate = rate,
        rate_type = "dsr",
        years_of_data = 3),
    paste0("the following arguments are required for rates: ",
           "data, numerator, rate, rate_type, years_of_data, multiplier"),
    info = "calculate_funnel_points input error; missing multiplier"
  )
})

# create df with inputs missing for some records
test_funnel_rate_funnels_input_NA <- test_funnel_rate_funnels_input %>%
  mutate(ev = case_when(between(pop, 5710, 5715) ~ NA_integer_,
                        TRUE ~ ev),
         rate = case_when(between(pop, 5710, 5715) ~ NA_integer_,
                          TRUE ~ rate),
         pop = case_when(between(pop, 5710, 5715) ~ NA_integer_,
                         TRUE ~ pop),

  )

test_funnel_rate_funnels_input_NA2 <- test_funnel_rate_funnels_input %>%
  mutate(ev = case_when(between(pop, 5710, 5715) ~ 0L,
                        TRUE ~ ev),
         rate = case_when(between(pop, 5710, 5715) ~ NA_integer_,
                          TRUE ~ rate),
         pop = case_when(between(pop, 5710, 5715) ~ NA_integer_,
                         TRUE ~ pop),

  )

test_that("calculate_funnel_points input error; denominator when numerator has 0 for rate", {
  expect_error(
    test_funnel_rate_funnels_input_NA2 %>%
      calculate_funnel_points(
        numerator = ev,
        rate = rate,
        rate_type = "dsr",
        years_of_data = 3,
        multiplier = 1e5),
    "for rates, where there are 0 events for a record, the denominator field needs to be provided using the denominator argument",
    info = "calculate_funnel_points input error; denominator when numerator has 0 for rate"
  )
  expect_error(
    test_funnel_rate_funnels_input_NA2 %>%
      calculate_funnel_points(
        numerator = ev,
        denominator = pop,
        rate = rate,
        rate_type = "dsr",
        years_of_data = 3,
        multiplier = 1e5),
    "for rates, where there are 0 events for a record, the denominator must be provided",
    info = "calculate_funnel_points input error; denominator when numerator has 0 for rate"
  )
})

test_that("calculate_funnel_limits input error; rates and proportions need input values for all records", {
  expect_error(
    test_funnel_rate_funnels_input_NA %>%
      calculate_funnel_limits(
        numerator = ev,
        rate = rate,
        rate_type = "crude",
        years_of_data = 1,
        multiplier = 1e5,
        statistic = "rate"),
    "for rates, numerators must be provided for all records, even when their values are zero",
    info = "calculate_funnel_limits input error; rates and proportions need input values for all records"
  )

  expect_error(
    test_funnel_rate_funnels_input_NA2 %>%
      calculate_funnel_limits(
        numerator = ev,
        rate = rate,
        rate_type = "dsr",
        years_of_data = 1,
        multiplier = 1e5,
        statistic = "rate"),
    "for rates, rates must be provided for all records, or a denominator must be provided if the rate is zero",
    info = "calculate_funnel_limits input error; rates and proportions need input values for all records"
  )

  expect_error(
    test_funnel_rate_funnels_input_NA2 %>%
      mutate(pop = case_when(ev == 0 ~ 0L,
                             TRUE ~ ev)) %>%
      calculate_funnel_limits(
        numerator = ev,
        denominator = pop,
        rate = rate,
        rate_type = "dsr",
        years_of_data = 1,
        multiplier = 1e5,
        statistic = "rate"),
    "for rates, where there are 0 events for a record, the denominator must be provided",
    info = "calculate_funnel_limits input error; rates and proportions need input values for all records"
  )

  expect_error(
    test_funnel_rate_funnels_input_NA2 %>%
      calculate_funnel_limits(
        numerator = ev,
        rate = rate,
        denominator = pop,
        rate_type = "dsr",
        years_of_data = 1,
        multiplier = 1e5,
        statistic = "rate"),
    "for rates, rates must be provided for all records, or a denominator must be provided if the rate is zero",
    info = "calculate_funnel_limits input error; rates and proportions need input values for all records"
  )

  expect_error(
    test_funnel_rate_funnels_input_NA %>%
      calculate_funnel_limits(
        numerator = ev,
        denominator = pop,
        multiplier = 1e5,
        statistic = "proportion"),
    "for proportions, numerators and denominators must be provided for all records, even when their values are zero",
    info = "calculate_funnel_limits input error; rates and proportions need input values for all records"
  )

})


test_that("assign_funnel_significance input error; rates and proportions need input values for all records", {
  expect_error(
    test_funnel_rate_funnels_input_NA2 %>%
      assign_funnel_significance(
        numerator = ev,
        rate = rate,
        rate_type = "crude",
        multiplier = 1e5,
        statistic = "rate"),
    paste0("for rates, rates must be provided for all records, ",
           "or a denominator must be provided if the rate is zero"),
    info = "calculate_funnel_limits input error; rates and proportions need input values for all records"
  )

  expect_error(
    test_funnel_rate_funnels_input_NA2 %>%
      mutate(pop = case_when(ev == 0 ~ 0L,
                             TRUE ~ ev)) %>%
      assign_funnel_significance(
        numerator = ev,
        denominator = pop,
        rate = rate,
        rate_type = "crude",
        multiplier = 1e5,
        statistic = "rate"),
    "for rates, where there are 0 events for a record, the denominator must be provided",
    info = "calculate_funnel_limits input error; rates and proportions need input values for all records"
  )

  expect_error(
    test_funnel_rate_funnels_input_NA2 %>%
      assign_funnel_significance(
        numerator = ev,
        denominator = pop,
        rate = rate,
        rate_type = "crude",
        multiplier = 1e5,
        statistic = "rate"),
    paste0("for rates, rates must be provided for all records, ",
           "or a denominator must be provided if the rate is zero"),
    info = "calculate_funnel_limits input error; rates and proportions need input values for all records"
  )

  expect_error(
    test_funnel_rate_funnels_input_NA %>%
      assign_funnel_significance(
        numerator = ev,
        rate = rate,
        rate_type = "crude",
        multiplier = 1e5,
        statistic = "rate"),
    "for rates, numerators must be provided for all records, even when their values are zero",
    info = "calculate_funnel_limits input error; rates and proportions need input values for all records"
  )

  expect_error(
    # create df with inputs missing for some records
    test_funnel_rate_funnels_input_NA %>%
      assign_funnel_significance(
        numerator = ev,
        rate = rate,
        rate_type = "dsr",
        multiplier = 1e5,
        statistic = "rate"),
    "for rates, numerators must be provided for all records, even when their values are zero",
    info = "calculate_funnel_limits input error; rates and proportions need input values for all records"
  )

  expect_error(
    # create df with inputs missing for some records
    test_funnel_rate_funnels_input_NA %>%
      assign_funnel_significance(
        numerator = ev,
        denominator = pop,
        multiplier = 1e5,
        statistic = "proportion"),
    "for proportions, numerators and denominators must be provided for all records, even when their values are zero",
    info = "calculate_funnel_limits input error; rates and proportions need input values for all records"
  )

})


