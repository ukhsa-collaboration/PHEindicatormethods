##TO DO

# check about whether grouping vars have to be factors?
# check whether age band as factor still works as R may r-order if factor used
# change message when count < 10 - "NA Count < 10"
# investigate tibbles versus data frames as outputs - currently some of each.
# add to documentation - something like  "these are the methods by which PHE calculate PHOF outcome framework indicators - ie PHE official methods in that sense.
# check if using normmean or students t when n is small
# check re using options(scipen=999) in function - does it work, is it good practice?  otherwise get "rate per 1e05"


# libraries
library(dplyr)
library(readxl)
library(tidyr)
library(rlang)
library(testthat)

# GROUPED FUNCTIONS

# View DSR input data (pre-grouped):
test_DSR_multiarea


# Execute DSR Function:

phe_dsr(test_DSR_multiarea, count, pop, esp2013)

phe_dsr(test_DSR_1976, count, pop, type="full", stdpop = test_DSR_1976$stdpop, multiplier=10000)

phe_dsr(test_DSR_multiarea, count, pop, type="full", stdpop = esp2013, multiplier=1000, conf.level = 99.8)


#Multiple Grouping categories:
Y2015 <- test_DSR_multiarea %>%
         mutate(Yr=2015)

Y2016 <- test_DSR_multiarea %>%
         mutate(Yr=2016,
                count = count + 10)

MultiGroups <- bind_rows(Y2015,Y2016)


phe_dsr(MultiGroups, count, pop, esp2013)

MultiGroups2 <- MultiGroups %>%
                group_by(area, Yr)

phe_dsr(MultiGroups2, count, pop, esp2013, type="full")


# UNGROUPED FUNCTIONS

# View Rate input data:
test_Rate_100_input <- test_Rate_100[1:4]
test_Rate_100_input

# Execute rate function:

phe_rate(test_Rate_100_input,Numerator,Denominator)

phe_rate(test_Rate_100_input,Numerator,Denominator, type = "full", conf.level=0.98, multiplier = 10000)


# Demo Proportion function:
prop <- data.frame(area = c("Area1","Area2","Area3"), numerator = c(125,82,100), denominator = c(100,100,100))
prop
phe_proportion(prop, x= numerator, n= denominator)
phe_proportion(prop, numerator, denominator, type="full")




#testing DSR error handling

#good code
phe_dsr(test_DSR_multiarea,count,pop,stdpop=esp2013)
phe_dsr(test_DSR_1976,count,pop,stdpop=esp2013[1:18])
phe_dsr(test_DSR_1976,count,pop,stdpop=test_DSR_1976$stdpop)
phe_dsr(test_DSR_1976,count,pop,stdpop)

#OR:
test <- test_DSR_1976 %>%
  + mutate(esp1976 = stdpop) %>%
  + select(-stdpop)
phe_dsr(test,count,pop,esp1976)

#bad code
phe_dsr(test_DSR_multiarea,count,pop,esp2013[18])
phe_dsr(test_DSR_multiarea,count,pop,test_DSR_1976$stdpop)
phe_dsr(slice(test_DSR_multiarea,54),count,pop,esp2013)
