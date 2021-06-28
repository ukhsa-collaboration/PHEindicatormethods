context("test_phe_funnels")


# test calculations
test_that("confidence limits calculate correctly",{
  funnel_table <- test_funnel_inputs %>%
    dplyr::select(numerator, denominator) %>%
    phe_funnels(numerator, denominator,
                type = "standard")
  expect_equal(funnel_table,
               test_funnel_outputs,
               info = "test default")



})

test_that("confidence limits calculate correctly with axis variation",{
  funnel_table <- test_funnel_inputs %>%
    dplyr::select(numerator, denominator) %>%
    filter(denominator < 31000) %>%
    phe_funnels(numerator, denominator)
  expect_equal(funnel_table,
               test_funnel_outputs_axis_variation,
               info = "test default with axis variation")



})

# test significance calculations
test_that("Significance for proportions calculates correctly", {
  expect_equal(data.frame(phe_funnel_significance(test_funnel_inputs[1:3], numerator, denominator)),
               data.frame(test_funnel_inputs),
               info = "test default"
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
