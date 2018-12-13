context("test_phe_quantiles")





#test calculations
test_that("quantiles calculate correctly",{
  expect_equal(phe_quantiles(test_quantiles,Value, AreaCode, ParentCode,
                                        invert = Polarity, inverttype = "field")[23],
               rename(test_quantiles[18],qname = DecileInRegion),check.attributes=FALSE, check.names=FALSE,info="test default")
})




function phe_quantiles requires at least 4 arguments: data, values, smallarea, highergeog
data must contain the same number of rows for each group
valid values for inverttype are vector and field
invert length must be a factor of the number of rows in each group within data
invert argument is not a field name from data
values argument must be a numeric field from data
invert values must take the same logical value for each data grouping set and highergeog


#test error handling
test_that("means - errors are generated when invalid arguments are used",{
  expect_error(phe_mean(test_Mean),
               "function phe_dsr requires at least 2 arguments: data, x",info="error invalid number of arguments")


})







# QA against Excel Tool

Tooldata <- read.csv(".\\fd_quantiles.csv", stringsAsFactors=FALSE) %>%
  select(IndicatorID, Sex, ParentCode, AreaCode, AreaName, Value, MaxValue,
         Rank.within.Region, Tool.output...number.of.areas, Tool.Output.Decile)
QAdf <- check %>%
  filter((IndicatorID == 40501 & Sex == "Female") |
           IndicatorID == 90366 & Sex == "Female") %>%
  select(IndicatorID, Sex, ParentCode, AreaCode, AreaName, Value, adj_value,
         rank, n, quantile) %>%
  full_join(Tooldata, by = c("IndicatorID", "Sex", "ParentCode","AreaCode")) %>%
  filter(round(Value.x,2) != round(Value.y,2) |
           rank != Rank.within.Region |
           quantile != Tool.Output.Decile)

