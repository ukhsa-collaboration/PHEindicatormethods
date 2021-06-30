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

# test error handling
test_that("incorrect statistic argument", {
  expect_error(
    test_funnel_inputs %>%
      dplyr::select(numerator, denominator) %>%
      filter(denominator < 31000) %>%
      phe_funnels(numerator, denominator,
                  statistic = "pop"),
    "'arg' should be one of \"proportion\"",
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
      x = obs,
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
      x = obs,
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
      x = obs,
      denominator = pop
    ),
    "numerators must be less than or equal to denominator for a proportion statistic",
    info = "error numerator > denominator"
  )
})

test_that("phe_funnel_significance requires data, x and denominator", {
  expect_error(
    phe_funnel_significance(
      data.frame(
        area = c("Area1", "Area2", "Area3"),
        pop = c(100, 100, 20)
      ), pop),
    "function phe_funnel_significance requires at least 3 arguments: data, x, n",
    info = "check the parameters passed into the function"
  )
})

test_that("phe_funnels requires data, x and denominator", {
  expect_error(
    phe_funnels(
      data.frame(
        area = c("Area1", "Area2", "Area3"),
        pop = c(100, 100, 20)
      ), pop),
    "function phe_funnels requires at least 3 arguments: data, x, n",
    info = "check the parameters passed into the function"
  )
})
