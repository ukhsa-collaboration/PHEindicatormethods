context("test_phe_fun_prop_plot")


# test calculations
test_that("confidence limits calculate correctly",{

  expect_equal(data.frame(phe_fun_prop_plot((test_fs)[2:3], numerator, denominator)),
               data.frame(test_fp),check.attributes=FALSE, check.names=FALSE, info="test default")


#  expect_equal(data.frame(phe_proportion(slice(test_Prop,9:16)[1:3], Numerator, Denominator,
#                              multiplier = 100, type="full")),
#                          data.frame(select(slice(test_Prop,9:16),1:9)),check.attributes=FALSE, check.names=FALSE, info="test full, percentage")

#  expect_equal(data.frame(phe_proportion(slice(test_Prop,1:8)[1:3], Numerator, Denominator,
#                              multiplier = 1, type="standard")),
#                          data.frame(select(slice(test_Prop,1:8),1:6)),check.attributes=FALSE, check.names=FALSE, info="test standard")

#  expect_equal(data.frame(phe_proportion(slice(test_Prop,17:24)[1:3], Numerator, Denominator,
#                              type="full", confidence=99.8)),
#               data.frame(select(slice(test_Prop,17:24),1:9)),check.attributes=FALSE, check.names=FALSE, info="test confidence")

#  expect_equal(data.frame(phe_proportion(slice(test_Prop,1:8)[1:3], Numerator, Denominator, type="value")),
#                          data.frame(select(slice(test_Prop,1:8),1:4)),check.attributes=FALSE, check.names=FALSE, info="test value")

#  expect_equal(data.frame(phe_proportion(slice(test_Prop,1:8)[1:3], Numerator, Denominator, type="lower")),
#                          data.frame(select(slice(test_Prop,1:8),1:3,5)),check.attributes=FALSE, check.names=FALSE, info="test lower")

#  expect_equal(data.frame(phe_proportion(slice(test_Prop,1:8)[1:3], Numerator, Denominator, type="upper")),
#               data.frame(select(slice(test_Prop,1:8),1:3,6)),check.attributes=FALSE, check.names=FALSE, info="test upper")

#  expect_equal(arrange(data.frame(phe_proportion(filter(test_Prop,Area %in% c("Area09","Area10","Area11"))[1:3], Numerator, Denominator, type="full")), Area),
#               arrange(data.frame(select(filter(test_Prop,Area %in% c("Area09","Area10","Area11")),1:9)), Area),check.attributes=FALSE, check.names=FALSE, info="test NAs")

#  expect_equal(arrange(data.frame(phe_proportion(slice(test_Prop_g,1:8)[1:3], Numerator, Denominator, type="standard")), Area),
#               arrange(data.frame(test_Prop_g_results[1:6]),Area),check.attributes=FALSE, check.names=FALSE, info="test grouped")

})


# test error handling
# check required arguments present
#  stop("function phe_proportion requires at least 3 arguments: data, x, n")
#  stop("numerators must be greater than or equal to zero")
#  stop("denominators must be greater than zero")
#  stop("numerators must be less than or equal to denominator for a proportion statistic")


test_that("denominators must be greater than zero",{

  expect_error(phe_fun_prop_sig(data.frame(area=c("Area1","Area2","Area3"),
                                         obs =c(65,80,30),
                                         pop =c(100,100,0)), obs,pop),
               "denominators must be greater than zero", info="check your denominators")

  expect_error(phe_fun_prop_sig(data.frame(area=c("Area1","Area2","Area3"),
                                         obs =c(65,-80,30),
                                        pop=c(100,100,200)
                                         ), obs, pop),
               "numerators must be greater than or equal to zero", info="error num < 0")

  expect_error(phe_fun_prop_sig(data.frame(area=c("Area1","Area2","Area3"),
                                         obs =c(65,80,30),
                                         pop =c(100,0,100)), obs, pop),
               "denominators must be greater than zero", info="error denom = 0")

  expect_error(phe_fun_prop_sig(data.frame(area=c("Area1","Area2","Area3"),
                                         obs =c(65,80,30),
                                         pop =c(100,100,-100)), obs, pop),
               "denominators must be greater than zero", info="error denom < 0")

  expect_error(phe_fun_prop_sig(data.frame(area=c("Area1","Area2","Area3"),
                                           obs =c(65,80,30),
                                           pop =c(100,100,20)), obs, pop),
               "numerators must be less than or equal to denominator for a proportion statistic", info="error numerator>denominator")

  expect_error(phe_fun_prop_sig(data.frame(area=c("Area1","Area2","Area3"),

                                           pop =c(100,100,20)), pop),
               "function phe_proportion requires at least 3 arguments: data, x, n", info="check the parameters passed into the function")

})
