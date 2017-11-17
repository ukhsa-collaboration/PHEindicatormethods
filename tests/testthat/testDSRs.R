# load libraries
library(readxl)

# import test data
DSR_testdata_small <- read_excel("DSR_TestData.xlsx", sheet=1, col_names=TRUE)
DSR_testdata_big <- read_excel("DSR_TestData.xlsx", sheet=3, col_names=TRUE)


# the following shows the epitools ageadjust.direct function works for crude rate and adjusted rate but is using a diff CI method
# (very slightly out compared to Tech brief 3).  think this uses exact method for all values??
# could still use this function but replace the CI calc so it uses exact method when numerator <389 (or 100 better ???) then use Byars for greater numerators  with the
ageadjust.direct(DSR_testdata_small$count, DSR_testdata_small$pop, rate = NULL, DSR_testdata_small$stdpop, conf.level = 0.95)


# dsrTest::dobsonControl - tests results to see which DSr Ci method ewas applied.
