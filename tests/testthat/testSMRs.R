context("test_phe_smr")

#test calculations
test_that("smrs and CIs calculate correctly",{

  expect_equal(data.frame(phe_smr(select(test_ISR_ownref,-refcount,-refpop), count, pop, x_ref = test_ISR_refdata$refcount, n_ref = test_ISR_refdata$refpop)),
               data.frame(select(slice(test_ISR_results,13:15),1,5:7)),
               check.attributes=FALSE, check.names=FALSE,info="test default")

  expect_equal(data.frame(phe_smr(test_ISR_ownref, count, pop, refcount, refpop, refpoptype="field")),
               data.frame(select(slice(test_ISR_results,13:15),1,5:7)),
               check.attributes=FALSE, check.names=FALSE,info="test default with own ref data by col name")

  expect_equal(data.frame(phe_smr(select(test_ISR_ownref,-refcount,-refpop), count, pop, test_ISR_ownref$refcount[1:19], test_ISR_ownref$refpop[1:19])),
               data.frame(select(slice(test_ISR_results,13:15),1,5:7)),
               check.attributes=FALSE, check.names=FALSE,info="test default with own ref data as vector")

  expect_equal(data.frame(phe_smr(test_err2, count, pop, x_ref = test_ISR_refdata$refcount, n_ref = test_ISR_refdata$refpop, refvalue=100)),
               data.frame(select(slice(test_ISR_results,27:28),1,5:7)),
               check.attributes=FALSE, check.names=FALSE,info="test n = 0")

  expect_equal(data.frame(phe_smr(select(test_ISR_ownref,-refcount,-refpop), count, pop,
                                  x_ref = c(10303,2824,NA,3615,3641,3490,3789,3213,3031,2771,3089,3490,3595,4745,5514,7125,5694,6210,5757),
                                  n_ref = c(50520,57173,60213,54659,44345,50128,62163,67423,62899,55463,60479,49974,44140,40888,37239,30819,18136,15325,13918))),
               data.frame(select(slice(test_ISR_results,13:15),1,5:7)),
               check.attributes=FALSE, check.names=FALSE,info="test ref as specified vector")

  expect_equal(data.frame(phe_smr(test_multiarea, count, pop, x_ref = test_ISR_refdata$refcount, n_ref = test_ISR_refdata$refpop, type="full")),
               data.frame(select(slice(test_ISR_results,13:15),1:3,5:10)),
               check.attributes=FALSE, check.names=FALSE,info="test full")

  expect_equal(data.frame(phe_smr(test_multiarea, count, pop, x_ref = test_ISR_refdata$refcount, n_ref = test_ISR_refdata$refpop, type="value")),
               data.frame(select(slice(test_ISR_results,13:15),1,5)),
               check.attributes=FALSE, check.names=FALSE,info="test value")

  expect_equal(data.frame(phe_smr(test_multiarea, count, pop, x_ref = test_ISR_refdata$refcount, n_ref = test_ISR_refdata$refpop, type="lower")),
               data.frame(select(slice(test_ISR_results,13:15),1,6)),
               check.attributes=FALSE, check.names=FALSE,info="test lower")

  expect_equal(data.frame(phe_smr(test_multiarea, count, pop, x_ref = test_ISR_refdata$refcount, n_ref = test_ISR_refdata$refpop,type="upper")),
               data.frame(select(slice(test_ISR_results,13:15),1,7)),
               check.attributes=FALSE, check.names=FALSE,info="test upper")

  expect_equal(data.frame(phe_smr(test_multiarea, count, pop, x_ref = test_ISR_refdata$refcount, n_ref = test_ISR_refdata$refpop,confidence = 99.8)),
               data.frame(select(slice(test_ISR_results,16:18),1,5:7)),
               check.attributes=FALSE, check.names=FALSE,info="test confidence")

  expect_equal(data.frame(phe_smr(test_multiarea, count, pop, x_ref = test_ISR_refdata$refcount, n_ref = test_ISR_refdata$refpop, refvalue=100)),
               data.frame(select(slice(test_ISR_results,19:21),1,5:7)),
               check.attributes=FALSE, check.names=FALSE,info="test refvalue")

})




# test error handling

test_that("smrs - errors are generated when invalid arguments are used",{

  expect_error(phe_smr(test_multiarea, count, pop),
               "function phe_smr requires at least 5 arguments: data, x, n, x_ref and n_ref",info="error invalid number of arguments")

  expect_error(phe_smr(test_err1, count, pop, x_ref = test_ISR_refdata$refcount, n_ref = test_ISR_refdata$refpop),
               "numerators must all be greater than or equal to zero",info="error numerators < 0")

  expect_error(phe_smr(test_err3, count, pop, x_ref = test_ISR_refdata$refcount, n_ref = test_ISR_refdata$refpop),
               "denominators must all be greater than or equal to zero",info="error denominator < 0")

  expect_error(phe_smr(test_multiarea, count, pop, x_ref = test_ISR_refdata$refcount, n_ref = test_ISR_refdata$refpop, confidence = 0.74),
               "confidence level must be between 90 and 100 or between 0.9 and 1",info="error confidence < 0.9")

  expect_error(phe_smr(test_multiarea, count, pop, x_ref = test_ISR_refdata$refcount, n_ref = test_ISR_refdata$refpop, confidence = 3),
               "confidence level must be between 90 and 100 or between 0.9 and 1",info="error confidence between 1 and 90")

  expect_error(phe_smr(test_multiarea, count, pop, x_ref = test_ISR_refdata$refcount, n_ref = test_ISR_refdata$refpop, confidence = 1000),
               "confidence level must be between 90 and 100 or between 0.9 and 1",info="error confidence >100")

  expect_error(phe_smr(test_multiarea, count, pop, x_ref = test_ISR_refdata$refcount, n_ref = test_ISR_refdata$refpop, type="combined"),
               "type must be one of value, lower, upper, standard or full",info="error invalid type")

  expect_error(phe_smr(filter(test_multiarea,count < 100), count, pop, x_ref = test_ISR_refdata$refcount, n_ref = test_ISR_refdata$refpop),
               "data must contain the same number of rows for each group",info="error num rows per group")

  expect_error(phe_smr(test_multiarea, count, pop, x_ref = test_ISR_refdata$refcount[1:18], n_ref = test_ISR_refdata$refpop),
               "x_ref length must equal number of rows in each group within data",info="error x_ref length")

  expect_error(phe_smr(test_multiarea, count, pop, x_ref = test_ISR_refdata$refcount, n_ref = test_ISR_refdata$refpop[2:19]),
               "n_ref length must equal number of rows in each group within data",info="error n_ref length")

  expect_error(phe_smr(test_ISR_ownref, count, pop, test_ISR_ownref$refcount, test_ISR_ownref$refpop[1:19]),
               "x_ref length must equal number of rows in each group within data",info="error x_ref length in data")

  expect_error(phe_smr(test_ISR_ownref, count, pop, test_ISR_ownref$refcount[1:19], test_ISR_ownref$refpop),
               "n_ref length must equal number of rows in each group within data",info="error n_ref length in data")

  expect_error(phe_smr(test_multiarea, count, pop, test_ISR_ownref$refcount[1:19], test_ISR_ownref$refpop, refpoptype = "column"),
               "valid values for refpoptype are vector and field",info="error invalid refpoptype")

  expect_error(phe_smr(test_ISR_ownref, count, pop, ref_count, refpop, refpoptype = "field"),
               "x_ref is not a field name from data",info="error x_ref not a fiel name")

  expect_error(phe_smr(test_ISR_ownref, count, pop, refcount, ref_pop, refpoptype = "field"),
               "n_ref is not a field name from data",info="error n_ref not a field name")
})


