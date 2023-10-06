#test calculations
test_that("byars_lower calculate correctly",{

  expect_equal(data.frame(lowercl = byars_lower(c(65,1045,1))),
               data.frame(slice(test_BW,1:3)[3]),ignore_attr=TRUE, info="test default")

  expect_equal(data.frame(lowercl = byars_lower(c(65,1045,10),confidence=99.8)),
               data.frame(slice(test_BW,4:6)[3]),ignore_attr=TRUE, info="test default")

  expect_equal(data.frame(lowercl = byars_upper(c(65,1045,1))),
               data.frame(slice(test_BW,1:3)[4]),ignore_attr=TRUE, info="test default")

  expect_equal(data.frame(lowercl = byars_upper(c(65,1045,10),confidence=0.998)),
               data.frame(slice(test_BW,4:6)[4]),ignore_attr=TRUE, info="test default")

  expect_equal(data.frame(lowercl = byars_upper(c(65,1045,10),confidence=99.8)),
               data.frame(slice(test_BW,4:6)[4]),ignore_attr=TRUE, info="test default")

})


# test error handling
test_that("byars_lower - errors are generated when invalid arguments are used",{
  expect_error(byars_lower(c(65,0,-4)),
               "observed events must all be greater than or equal to zero", info="error obs < 0")
  expect_error(byars_lower(c(65,80,1000), confidence = 0.7),
               "confidence level must be between 90 and 100 or between 0.9 and 1", info="error confidence < 0.9")
  expect_error(byars_lower(c(65,80,1000), confidence = 14),
               "confidence level must be between 90 and 100 or between 0.9 and 1", info="error confidence between 1 and 90")
  expect_error(byars_lower(c(65,80,1000), confidence = 125),
               "confidence level must be between 90 and 100 or between 0.9 and 1", info="error confidence > 100")
  expect_error(byars_upper(c(65,0,-4)),
                 "observed events must all be greater than or equal to zero", info="error obs < 0")
  expect_error(byars_upper(c(65,80,1000), confidence = 0.7),
                 "confidence level must be between 90 and 100 or between 0.9 and 1", info="error confidence < 0.9")
  expect_error(byars_upper(c(65,80,1000), confidence = 14),
                 "confidence level must be between 90 and 100 or between 0.9 and 1", info="error confidence between 1 and 90")
  expect_error(byars_upper(c(65,80,1000), confidence = 125),
                 "confidence level must be between 90 and 100 or between 0.9 and 1", info="error confidence > 100")
  })


