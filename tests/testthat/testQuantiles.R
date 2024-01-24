# test grouped df field
df1 <- test_quantiles_g %>% filter(GroupSet == "IndSexReg") %>%
  group_by(IndSexRef, ParentCode)

# test grouped df logical
df2 <- test_quantiles_g %>% filter(IndSexRef == "90366Female"& GroupSet == "IndSexReg") %>%
  group_by(IndSexRef, ParentCode)

# test ungrouped df field
df3 <- test_quantiles_g %>% filter(GroupSet == "IndSex")

# test ungrouped df logical
df4 <- test_quantiles_ug %>% filter(GroupSet == "None")

# test grouped
df5 <- df4 %>% group_by(GroupSet)

# test data where all values are NA
df6 <- df2 |> filter(!AreaCode %in% c("E06000053", "E09000001"))|>
  mutate(Value = case_when(ParentCode == "E12000006" ~ NA_real_,
                           TRUE ~ Value))

#test calculations
test_that("quantiles calculate correctly",{

  suppressWarnings({
    # within-region deciles for multiple indicators
    expect_equal(data.frame(phe_quantile(df1,Value,
                                         invert = Polarity, inverttype = "field")[15]),
                 rename(df1,quantile = QuantileInGrp)[14],
                 ignore_attr = TRUE,info="test grouped df field")
    # within-region deciles for multiple indicators
    expect_equal(data.frame(phe_quantile(df2,Value,
                                         invert = FALSE))[15:18],
                 data.frame(tibble(quantile = df2$QuantileInGrp,
                                   nquantiles = 10L,
                                   groupvars = "IndSexRef, ParentCode",
                                   qinverted = "lowest quantile represents lowest values")),
                 ignore_attr = TRUE,info="test grouped df logical")

    expect_equal(phe_quantile(df3, Value,
                              invert = Polarity, inverttype = "field", nquantiles = 7L)[15],
                 rename(df3,quantile = QuantileInGrp)[14],
                 ignore_attr = TRUE,info="test ungrouped df field")

    expect_equal(phe_quantile(df5, Value, nquantiles = 4L)[15],
                 rename(df4,quantile = QuantileInGrp)[14],
                 ignore_attr = TRUE,info="test nquantiles")

    expect_equal(phe_quantile(df4, Value, nquantiles = 4L)[15],
                 rename(df4,quantile = QuantileInGrp)[14],
                 ignore_attr = TRUE,info="test ungrouped df logical nohighergeog")

    expect_equal(phe_quantile(df4, Value, nquantiles = 4L, type="standard")[15],
               rename(df4,quantile = QuantileInGrp)[14],
               ignore_attr = TRUE,info="test ungrouped df logical nohighergeog")

  })
})

#test warnings
test_that("quantiles - warnings are generated when too few small areas for number of quantiles",{
  expect_warning(data.frame(phe_quantile(df2, Value, invert = FALSE)),
               "One or more groups had too few small areas with values to allow quantiles to be assigned",
               info="warning too few small areas")

  expect_warning(data.frame(phe_quantile(df6, Value, invert = FALSE)),
                 "One or more groups had too few small areas with values to allow quantiles to be assigned",
                 info="warning too few small areas")
})

#test error handling
test_that("quantiles - errors are generated when invalid arguments are used",{
  expect_error(phe_quantile(test_quantiles_g),
               "function phe_quantile requires at least 2 arguments: data and values",
               info="error invalid number of arguments")
  expect_error(phe_quantile(test_quantiles_g, Value,
                            invert = Polarity, inverttype = "vector"),
               "valid values for inverttype are logical and field",info="error inverttype is invalid")
  expect_error(phe_quantile(test_quantiles_g, Value,
                            invert = 6, inverttype = "logical"),
               "invert expressed as a logical must equal TRUE or FALSE",info="error logical invert is invalid")
  expect_error(phe_quantile(test_quantiles_g, AreaName,
                            invert = Polarity, inverttype = "field"),
               "values argument must be a numeric field from data",info="error value is invalid")
  expect_error(phe_quantile(test_quantiles_fail, Value,
                            invert = Polarity, inverttype = "field"),
               "invert field values must take the same logical value for each data grouping set",info="error invert varies")
  expect_error(phe_quantile(test_quantiles_fail, Value,
                            invert = Pol, inverttype = "field"),
               "invert is not a field name from data",info="error invert not valid field name")
  })
