context("test_phe_SII")

# 1) Test calculations ----------------------------------------------------
# Expect SII value to match exactly, and confidence limits to be within
# given tolerance

# Set tolerance for confidence limits
tol <- 0.05

# Set lower number of repetitions for faster testing
no_reps <- 10000


# Run tests ---------------------------------------------------------------

# Test function gives same confidence limits when seed is set and
# run on same data

# Set seed to determine random number generation in simulation function
set.seed(5)

test1 <- data.frame(phe_sii(SII_test_grouped[1:20, 3:13],
                            Quantile, Population,
                            value_type = 0, # default normal distribution
                            value = Value, # value supplied
                            se = StandardError,
                            repetitions = 100,
                            rii = TRUE,
                            type = "standard"))

set.seed(5)

test2 <- data.frame(phe_sii(SII_test_grouped[1:20, 3:13],
                            Quantile, Population,
                            value_type = 0, # default normal distribution
                            value = Value, # value supplied
                            se = StandardError,
                            repetitions = 100,
                            rii = TRUE,
                            type = "standard"))

expect_equal(test1, test2)

test_that("SII and confidence limits calculate correctly",{


# ***************************************
# *** DEFAULT NORMAL (value_type = 0) ***
# ***************************************

    # test SII default calculation on 2 areas, supplying value field
  expect_equal(data.frame(phe_sii(SII_test_grouped[1:20, 3:13],
                                  Quantile, Population,
                                  value_type = 0, # default normal distribution
                                  value = Value, # value supplied
                                  se = StandardError,
                                  repetitions = no_reps,
                                  rii = TRUE,
                                  type = "standard")),
               data.frame(SII_test_grouped[c(1,11), c(3:5,16:21)]),
               check.attributes=FALSE, check.names=FALSE,
               info="test default with SE provided", tolerance = tol)

   # test same calculation, supplying upper and lower CLs rather than SE
  expect_equal(data.frame(phe_sii(SII_test_grouped[1:20, 3:13],
                                        Quantile, Population,
                                        value_type = 0,
                                        value = Value,
                                        lower_cl = LowerCL, # CLs supplied
                                        upper_cl = UpperCL,
                                        repetitions = no_reps,
                                        rii = TRUE,
                                  type = "standard")),
                     data.frame(SII_test_grouped[c(1,11), c(3:5,16:21)]),
                     check.attributes=FALSE, check.names=FALSE,
                     info="test default with CLs provided", tolerance = tol)

   # test function on ungrouped dataset
  expect_equal(data.frame(phe_sii(SII_test_data[1:10, 3:13],
                                  Quantile, Population,
                                  value_type = 0,
                                  value = Value,
                                  lower_cl = LowerCL, # CLs supplied
                                  upper_cl = UpperCL,
                                  repetitions = no_reps,
                                  rii = TRUE,
                                  type = "standard")),
               data.frame(SII_test_grouped[1, c(16:21)]),
               check.attributes=FALSE, check.names=FALSE,
               info="test on ungrouped data", tolerance = tol)

    # test SII calculation at 99% confidence (inputted as decimal)
  expect_equal(data.frame(phe_sii(SII_test_grouped[21:40, 3:13],
                                  Quantile, Population,
                                  value_type = 0,
                                  value = Value,
                                  lower_cl = LowerCL,
                                  upper_cl = UpperCL,
                                  confidence = 0.99,
                                  repetitions = no_reps,
                                  rii = TRUE,
                                  type = "standard")), # SII confidence changed
               data.frame(SII_test_grouped[c(21,31), c(3:5,16:21)]),
               check.attributes=FALSE, check.names=FALSE,
               info="test at 99% confidence (decimal)", tolerance = tol)

    # test SII calculation at 99% confidence (inputted as %)
  expect_equal(data.frame(phe_sii(SII_test_grouped[21:40, 3:13],
                                  Quantile, Population,
                                  value_type = 0,
                                  value = Value,
                                  se = StandardError,
                                  confidence = 99,
                                  repetitions = no_reps,
                                  rii = TRUE,
                                  type = "standard")), # SII confidence changed
               data.frame(SII_test_grouped[c(21,31), c(3:5,16:21)]),
               check.attributes=FALSE, check.names=FALSE,
               info="test at 99% confidence (%)", tolerance = tol)

    # test SII calculation on 100,000 repetitions
  expect_equal(data.frame(phe_sii(SII_test_grouped[41:60, 3:13],
                                  Quantile, Population,
                                  value_type = 0,
                                  value = Value,
                                  se = StandardError,
                                  repetitions = 100000,
                                  rii = TRUE,
                                  type = "standard")), # No. repetitions changed
               data.frame(SII_test_grouped[c(41,51), c(3:5,16:21)]),
               check.attributes=FALSE, check.names=FALSE,
               info="test on 10000 repetitions", tolerance = tol)

  # test SII calculation on quintiles instead of deciles
  expect_equal(data.frame(phe_sii(SII_test_grouped[61:70, 3:13],
                                  Quantile, Population,
                                  value_type = 0,
                                  value = Value,
                                  se = StandardError,
                                  repetitions = no_reps,
                                  rii = TRUE,
                                  type = "standard"))[, 1:4], # only have SII available from Excel tool
               data.frame(SII_test_grouped[c(61,66), c(3:5,16)]),
               check.attributes=FALSE, check.names=FALSE,
               info="test on quintiles", tolerance = tol)

  # *****************************
  # *** RATE (value_type = 1) ***
  # *****************************

  # test SII rate calculation on 2 areas, supplying SE
  expect_equal(data.frame(phe_sii(SII_test_grouped[71:90, 3:13],
                                  Quantile, Population,
                                  value_type = 1, # rate indicator
                                  value = Value,
                                  se = StandardError,
                                  repetitions = no_reps,
                                  rii = TRUE,
                                  type = "standard")),
               data.frame(SII_test_grouped[c(71,81), c(3:5,16:21)]),
               check.attributes=FALSE, check.names=FALSE,
               info="test rate with SE provided", tolerance = tol)

  # test same calculation, supplying upper and lower CLs (before transformation)
  # rather than SE
  expect_equal(data.frame(phe_sii(SII_test_grouped[71:90, 3:13],
                                  Quantile, Population,
                                  value_type = 1,
                                  value = Value,
                                  lower_cl = LowerCL, # CLs supplied
                                  upper_cl = UpperCL,
                                  repetitions = no_reps,
                                  rii = TRUE,
                                  type = "standard")),
               data.frame(SII_test_grouped[c(71,81), c(3:5,16:21)]),
               check.attributes=FALSE, check.names=FALSE,
               info="test rate with CLs provided", tolerance = tol)


  # ***********************************
  # *** PROPORTION (value_type = 2) ***
  # ***********************************

  # test SII rate calculation on 2 areas, supplying SE
  expect_equal(data.frame(phe_sii(SII_test_grouped[91:110, 3:13],
                                  Quantile, Population,
                                  value_type = 2, # rate indicator
                                  value = Value,
                                  se = StandardError,
                                  repetitions = no_reps,
                                  rii = TRUE,
                                  type = "standard")),
               data.frame(SII_test_grouped[c(91,101), c(3:5,16:21)]),
               check.attributes=FALSE, check.names=FALSE,
               info="test proportion with SE provided", tolerance = tol)

  # test same calculation, supplying upper and lower CLs (before transformation)
  # rather than SE
  expect_equal(data.frame(phe_sii(SII_test_grouped[91:110, 3:13],
                                  Quantile, Population,
                                  value_type = 2,
                                  value = Value,
                                  lower_cl = LowerCL, # CLs supplied
                                  upper_cl = UpperCL,
                                  repetitions = no_reps,
                                  rii = TRUE,
                                  type = "standard")),
               data.frame(SII_test_grouped[c(91,101), c(3:5,16:21)]),
               check.attributes=FALSE, check.names=FALSE,
               info="test proportion with CLs provided", tolerance = tol)

  # test same calculation, supplying count instead of value
  expect_equal(data.frame(phe_sii(SII_test_grouped[91:110, 3:13],
                                  Quantile, Population,
                                  value_type = 2,
                                  x = Count, # count supplied
                                  lower_cl = LowerCL,
                                  upper_cl = UpperCL,
                                  repetitions = no_reps,
                                  rii = TRUE,
                                  type = "standard")),
               data.frame(SII_test_grouped[c(91,101), c(3:5,16:21)]),
               check.attributes=FALSE, check.names=FALSE,
               info="test proportion with count provided", tolerance = tol)

  # test SII calculation with multiplier applied to outputs
  expect_equal(data.frame(phe_sii(SII_test_grouped[111:130, 3:13],
                                  Quantile, Population,
                                  value_type = 2,
                                  value = Value,
                                  multiplier = 100, # multiplier applied
                                  lower_cl = LowerCL,
                                  upper_cl = UpperCL,
                                  repetitions = no_reps,
                                  rii = TRUE,
                                  type = "standard")),
               data.frame(SII_test_grouped[c(111,121), c(3:5,16:21)]),
               check.attributes=FALSE, check.names=FALSE,
               info="test proportion with multiplier", tolerance = tol)

})


# 2) Test error handling --------------------------------------------------

test_that("errors are generated when invalid parameters are entered",{
        # missing population parameter (obligatory input)
  expect_error(phe_sii(SII_test_grouped[1:20, 3:13],
                                  Quantile,
                                  x = Count,
                                  se = StandardError),
               "function phe_sii requires the arguments: data, quantile, population",
               info = "missing population parameter")
        # missing quantile parameter (obligatory input)
  expect_error(phe_sii(SII_test_grouped[1:20, 3:13],
                             Population,
                             x = Count,
                             se = StandardError),
               "function phe_sii requires the arguments: data, quantile, population",
               info = "missing quantile parameter")
        # missing value parameter (obligatory input) - for default
  expect_error(phe_sii(SII_test_grouped[1:20, 3:13],
                       Quantile,
                       Population,
                       value_type = 0,
                       se = StandardError),
               "function phe_sii requires value field, or x field if indicator is a proportion of population",
               info = "missing value parameter - for default")
       # missing value parameter (obligatory input) - for rate
  expect_error(phe_sii(SII_test_grouped[71:90, 3:13],
                       Quantile,
                       Population,
                       value_type = 1,
                       se = StandardError),
               "function phe_sii requires value field, or x field if indicator is a proportion of population",
               info = "missing value parameter - for rate")
        # missing count and value parameters - for proportions
  expect_error(phe_sii(SII_test_grouped[91:110, 3:13],
                       Quantile,
                       Population,
                       value_type = 2,
                       se = StandardError),
               "function phe_sii requires value field, or x field if indicator is a proportion of population",
               info = "missing count and value parameters - for proportions")
        # missing standard error and a confidence limit parameter
  expect_error(phe_sii(SII_test_grouped[1:20, 2:10],
                       Quantile,
                       Population,
                       value = Value,
                       lower_cl = LowerCI),
               "function phe_sii requires either lower_cl and upper_cl fields, or se field",
               info = "missing standard error and a confidence limit parameter")
        # invalid indicator value type - string
  expect_error(phe_sii(SII_test_grouped[1:20, 3:13],
                       Quantile,
                       Population,
                       value = Value,
                       value_type = "rate",
                       se = StandardError,
                       repetitions = 0),
               "value_type should be 0, 1 or 2",
               info = "invalid indicator value type - string")
       # invalid indicator value type - integer
  expect_error(phe_sii(SII_test_grouped[1:20, 3:13],
                       Quantile,
                       Population,
                       value = Value,
                       value_type = 3,
                       se = StandardError),
               "value_type should be 0, 1 or 2",
               info = "invalid indicator value type - integer")
        # invalid multiplier - string
  expect_error(phe_sii(SII_test_grouped[1:20, 3:13],
                       Quantile,
                       Population,
                       value = Value,
                       se = StandardError,
                       multiplier = "none"),
               "multiplier, repetitions and confidence inputs should be numeric",
               info = "invalid multiplier - string")
        # invalid number of repetitions - numeric
  expect_error(phe_sii(SII_test_grouped[1:20, 3:13],
                       Quantile,
                       Population,
                       value = Value,
                       se = StandardError,
                       repetitions = 0),
               "number of repetitions must be greater than 0. Default is 100,000",
               info = "invalid number of repetitions - numeric")
        # invalid number of repetitions - string
  expect_error(phe_sii(SII_test_grouped[1:20, 3:13],
                       Quantile,
                       Population,
                       value = Value,
                       se = StandardError,
                       repetitions = "none"),
               "multiplier, repetitions and confidence inputs should be numeric",
               info = "invalid number of repetitions - string")
        # invalid confidence limit (decimal)
  expect_error(phe_sii(SII_test_grouped[1:20, 3:13],
                       Quantile,
                       Population,
                       value = Value,
                       se = StandardError,
                       confidence = 0.35),
               "confidence level for SII must be between 50 and 99.99, or between 0.5 and 0.9999",
               info = "invalid confidence limit (decimal)")
        # invalid confidence limit (%) - too low
  expect_error(phe_sii(SII_test_grouped[1:20, 3:13],
                       Quantile,
                       Population,
                       value = Value,
                       se = StandardError,
                       confidence = 40),
               "confidence level for SII must be between 50 and 99.99, or between 0.5 and 0.9999",
               info = "invalid confidence limit pc - too low")
        # invalid confidence limit (%) - too high
  expect_error(phe_sii(SII_test_grouped[1:20, 3:13],
                       Quantile,
                       Population,
                       value = Value,
                       se = StandardError,
                       confidence = 135),
               "confidence level for SII must be between 50 and 99.99, or between 0.5 and 0.9999",
               info = "invalid confidence limit pc - too high")

})


test_that("errors are generated when input dataframe contains invalid data",{

        # negative populations in dataset
  expect_error(phe_sii(SII_test_grouped[131:140, 3:13],
                       Quantile,
                       Population,
                       value = Value,
                       se = StandardError),
               "some groups have a zero or negative population")
        # negative SE in dataset
  expect_error(phe_sii(SII_test_grouped[141:145, 3:13],
                             Quantile,
                             Population,
                             value = Value,
                             se = StandardError),
               "some groups have a negative standard error")
        # zero count in dataset
  expect_error(phe_sii(SII_test_grouped[146:150, 3:13],
                             Quantile,
                             Population,
                             x = Count,
                             value_type = 2,
                             se = StandardError),
               "some groups have a zero or negative count x")
        # value proportions outside 0 to 1 range
  expect_error(phe_sii(SII_test_grouped[1:20, 3:13],
                       Quantile,
                       Population,
                       value = Value,
                       value_type = 2,
                       se = StandardError),
               "value proportions are not all between 0 and 1")
        # CL proportions outside 0 to 1 range
  expect_error(phe_sii(SII_test_grouped[151:155, 3:13],
                       Quantile,
                       Population,
                       value = Value,
                       value_type = 2,
                       lower_cl = LowerCL,
                       upper_cl = UpperCL),
               "confidence limit proportions are not all between 0 and 1")
  expect_error(phe_sii(SII_test_grouped[156:160, 3:13],
                       Quantile,
                       Population,
                       value = Value,
                       value_type = 2,
                       lower_cl = LowerCL,
                       upper_cl = UpperCL),
               "confidence limit proportions are not all between 0 and 1")
  # invalid number of quantiles (too small)
  expect_error(phe_sii(SII_test_grouped[1:3, 3:13],
                       Quantile,
                       Population,
                       value = Value,
                       lower_cl = LowerCL,
                       upper_cl = UpperCL),
               "Number of quantiles must be between 5 and 100")

})



# 3) Test warnings --------------------------------------------------------

test_that("warnings are generated when data does not pass quality checks",{

        # incomplete or invalid records
        expect_warning(phe_sii(SII_test_grouped[c(1:20, 161:170), 3:13],
                             Quantile,
                             Population,
                             value = Value,
                             lower_cl = LowerCL,
                             upper_cl = UpperCL,
                             type = "standard"),
                     "some records have been removed due to incomplete or invalid data")

        # TO ADD - reliability stat tolerance breached
        #####
})



# 4) Test reliability stats  ----------------------------------------------

test_that("reliaibility stats are returned when requested",{

        # check dimensions WITH reliability stats
        expect_equal(dim(TEST <- phe_sii(SII_test_grouped[1:20, 3:13],
                               Quantile,
                               Population,
                               value = Value,
                               lower_cl = LowerCL,
                               upper_cl = UpperCL,
                               repetitions = 100,
                               reliability_stat = TRUE,
                               type = "standard")),
                       c(2,7))

        # check dimensions WITHOUT reliability stats
        expect_equal(dim(phe_sii(SII_test_grouped[1:20, 3:13],
                                 Quantile,
                                 Population,
                                 value = Value,
                                 lower_cl = LowerCL,
                                 upper_cl = UpperCL,
                                 repetitions = 100,
                                 reliability_stat = FALSE,
                                 type = "standard")),
                     c(2,6))

        # check dimensions with default type = "full"
        expect_equal(dim(phe_sii(SII_test_grouped[1:20, 3:13],
                                 Quantile,
                                 Population,
                                 value = Value,
                                 lower_cl = LowerCL,
                                 upper_cl = UpperCL,
                                 repetitions = 100,
                                 reliability_stat = FALSE,
                                 type = "full")),
                     c(2,10))
})
