#test calculations
test_that("wilson_lower calculate correctly",{

  expect_equal(data.frame(lowercl = wilson_lower(c(65,1045,0.445),c(100,5000,1))),
               data.frame(slice(test_BW,7:9)[3]),ignore_attr = TRUE, info="test default")

  expect_equal(data.frame(lowercl = wilson_lower(c(65,1045,0.445),c(100,5000,1),confidence=99.8)),
               data.frame(slice(test_BW,10:12)[3]),ignore_attr = TRUE, info="test default")

  expect_equal(data.frame(lowercl = wilson_lower(c(65,1045,0.445),c(100,5000,1),confidence=1)),
               data.frame(uppercl = 0),ignore_attr = TRUE, info="test default")

  expect_equal(data.frame(uppercl = wilson_upper(c(65,1045,0.445),c(100,5000,1))),
               data.frame(slice(test_BW,7:9)[4]),ignore_attr = TRUE, info="test default")

  expect_equal(data.frame(uppercl = wilson_upper(c(65,1045,0.445),c(100,5000,1),confidence=0.998)),
               data.frame(slice(test_BW,10:12)[4]),ignore_attr = TRUE, info="test default")

  expect_equal(data.frame(uppercl = wilson_upper(c(65,1045,0.445),c(100,5000,1),confidence=1)),
               data.frame(uppercl = 1),ignore_attr = TRUE, info="test default")

  expect_equal(data.frame(uppercl = wilson_upper(c(65,1045,0.445),c(100,5000,1),confidence=99.8)),
               data.frame(slice(test_BW,10:12)[4]),ignore_attr = TRUE, info="test default")
})


# test error handling
test_that("wilson_lower - errors are generated when invalid arguments are used",{
  expect_error(wilson_lower(c(65,0,-4),c(100,5000,1)),
               "observed events must all be greater than or equal to zero", info="wilson_lower error obs < 0")
  expect_error(wilson_lower(c(65,80,1000),c(100,5000,-6)),
               "sample sizes must all be greater than zero", info="wilson_lower error pop <= 0")
  expect_error(wilson_lower(c(65,80,1000),c(100,50,5000)),
               "numerators must be less than or equal to denominator for a Wilson score to be calculated", info="wilson_lower x > n")
  expect_error(wilson_lower(c(65,80,1000),c(100,100,5000), confidence = 0.7),
               "confidence level must be between 90 and 100 or between 0.9 and 1", info="wilson_lower error confidence < 0.9")
  expect_error(wilson_lower(c(65,80,1000),c(100,100,5000), confidence = 14),
               "confidence level must be between 90 and 100 or between 0.9 and 1", info="wilson_lower error confidence between 1 and 90")
  expect_error(wilson_lower(c(65,80,1000),c(100,100,5000), confidence = 125),
               "confidence level must be between 90 and 100 or between 0.9 and 1", info="wilson_lower error confidence > 100")
  expect_error(wilson_upper(c(65,0,-4),c(100,100,5000)),
               "observed events must all be greater than or equal to zero", info="wilson_upper error obs < 0")
  expect_error(wilson_upper(c(65,80,1000),c(100,100,-6)),
               "sample sizes must all be greater than zero", info="wilson_upper error pop <= 0")
  expect_error(wilson_upper(c(65,80,1000),c(100,50,5000)),
               "numerators must be less than or equal to denominator for a Wilson score to be calculated", info="wilson_upper x > n")
  expect_error(wilson_upper(c(65,80,1000),c(100,100,5000), confidence = 0.7),
               "confidence level must be between 90 and 100 or between 0.9 and 1", info="wilson_upper error confidence < 0.9")
  expect_error(wilson_upper(c(65,80,1000),c(100,100,5000), confidence = 14),
               "confidence level must be between 90 and 100 or between 0.9 and 1", info="wilson_upper error confidence between 1 and 90")
  expect_error(wilson_upper(c(65,80,1000),c(100,100,5000), confidence = 125),
               "confidence level must be between 90 and 100 or between 0.9 and 1", info="wilson_upper error confidence > 100")
})


