dt<-read.csv("C:/Users/matthew.francis/Documents/R/Projects/PHEindicatormethods/tests/testthat/FunnelPlotTest.csv")

check<-phe_prop_funnel_sig(dt,numerator,denominator,100)


# calculate proportion and CIs
# initialise the variables
# create summary variables with dplyr
tot_x<-dt%>%
  summarise(tot_x = sum({{ x }}))

tot_n<-dt%>%
  summarise(tot_n = sum({{ denom }}))

# extract the values from the dataframes
tot_x <- tot_x$tot_x
tot_n <- tot_n$tot_n

# calculate the average proportion
tot_av<-tot_x/tot_n*multiplier

# write the function
dt_sig<-dt%>%
  mutate(low0_001 = max(0,((tot_av/multiplier*({{ denom }}/qnorm(0.999)^2+1)-
                              sqrt((-8*tot_av/multiplier*({{ denom }}/qnorm(0.999)^2+1))^2-
                                     64*(1/qnorm(0.999)^2 +1/{{ denom }})*tot_av/multiplier*
                                     ({{ denom }}*(tot_av/multiplier*({{ denom }}/qnorm(0.999)^2+2)-1)+
                                        qnorm(0.999)^2*(tot_av/multiplier-1)))/
                              8)/(1/qnorm(0.999)^2 +1/{{ denom }})/{{ denom }})*multiplier),
         low0_025 = max(0,((tot_av/multiplier*({{ denom }}/qnorm(0.975)^2+1)-
                              sqrt((-8*tot_av/multiplier*({{ denom }}/qnorm(0.975)^2+1))^2-
                                     64*(1/qnorm(0.975)^2 +1/{{ denom }})*tot_av/multiplier*
                                     ({{ denom }}*(tot_av/multiplier*({{ denom }}/qnorm(0.975)^2+2)-1)+
                                        qnorm(0.975)^2*(tot_av/multiplier-1)))/
                              8)/(1/qnorm(0.975)^2 +1/{{ denom }})/{{ denom }})*multiplier),
         high0_001 = min(multiplier,((tot_av/multiplier*({{ denom }}/qnorm(0.999)^2+1)+
                                        sqrt((-8*tot_av/multiplier*({{ denom }}/qnorm(0.999)^2+1))^2-
                                               64*(1/qnorm(0.999)^2 +1/{{ denom }})*tot_av/multiplier*
                                               ({{ denom }}*(tot_av/multiplier*({{ denom }}/qnorm(0.999)^2+2)-1)+
                                                  qnorm(0.999)^2*(tot_av/multiplier-1)))/
                                        8)/(1/qnorm(0.999)^2 +1/{{ denom }})/{{ denom }})*multiplier),
         high0_025 = min(multiplier,((tot_av/multiplier*({{ denom }}/qnorm(0.975)^2+1)+
                                        sqrt((-8*tot_av/multiplier*({{ denom }}/qnorm(0.975)^2+1))^2-
                                               64*(1/qnorm(0.975)^2 +1/{{ denom }})*tot_av/multiplier*
                                               ({{ denom }}*(tot_av/multiplier*({{ denom }}/qnorm(0.975)^2+2)-1)+
                                                  qnorm(0.975)^2*(tot_av/multiplier-1)))/
                                        8)/(1/qnorm(0.975)^2 +1/{{ denom }})/{{ denom }})*multiplier)) %>%
  mutate(significance = case_when(
    {{ x }}/{{ denom }}*multiplier<low0_001  ~ "Low (0.001)",

    {{ x }}/{{ denom }}*multiplier<low0_025  ~ "Low (0.025)",

    {{ x }}/{{ denom }}*multiplier>high0_001 ~ "High (0.001)",

    {{ x }}/{{ denom }}*multiplier>high0_025 ~ "High (0.025)",  TRUE ~ "")
  ) %>%
  select(-low0_001, -low0_025, -high0_001, -high0_025)


####################


# prop_calc<-prop*100
#  av<-sum({{x}})/sum({{denom}})
#  mind<-min({{denom}})
#  maxd<-max({{denom}})
#  actmind<- if(maxd>2*mind){0
#  } else {floor(mind*0.95)}
#  actmaxd<- ceiling(maxd)
#  minp<-min(prop_calc)
#  maxp<-max(prop_calc)
#  actminp<- if(maxp>2*minp){0
#  } else {floor(minp*0.95)}

# First create a vector with the numbers 1 to 100 (as the plot will have 100 data points)

#Create a blank matrix which will be populated with results obtained above

#  t<-matrix(ncol=7,nrow=100)
#  colnames(t)<-c("Row.number","Population","Lower2s0025limit","Upper2s0025limit","Lower3s0001limit",
#                 "Upper3s0001limit","Baseline")
#  t[1,1]=1
#
#  t[1,2]=1
#  t[1,3]= max(0,((av*(t[1,2]/qnorm(0.975)^2+1)-sqrt((-8*av*(t[1,2]/qnorm(0.975)^2+1))^2-64*(1/qnorm(0.975)^2+1/t[1,2])*av*(t[1,2]*(av*(t[1,2]/qnorm(0.975)^2+2)-1)+qnorm(0.975)^2*
#                                                                                                                             (av-1)))/8)/(1/qnorm(0.975)^2+1/t[1,2])/t[1,2])*100)
#  t[1,4]= min(100,((av*(t[1,2]/qnorm(0.975)^2+1)+sqrt((-8*av*(t[1,2]/qnorm(0.975)^2+1))^2-64*(1/qnorm(0.975)^2+1/t[1,2])*av*(t[1,2]*(av*(t[1,2]/qnorm(0.975)^2+2)-1)+qnorm(0.975)^2*
#                                                                                                                               (av-1)))/8)/(1/qnorm(0.975)^2+1/t[1,2])/t[1,2])*100)
#
#  t[1,5]= max(0,((av*(t[1,2]/qnorm(0.999)^2+1)-sqrt((-8*av*(t[1,2]/qnorm(0.999)^2+1))^2-64*(1/qnorm(0.999)^2+1/t[1,2])*av*(t[1,2]*(av*(t[1,2]/qnorm(0.999)^2+2)-1)+qnorm(0.999)^2*
#                                                                                                                             (av-1)))/8)/(1/qnorm(0.999)^2+1/t[1,2])/t[1,2])*100)
#
#  t[1,6]= min(100,((av*(t[1,2]/qnorm(0.999)^2+1)+sqrt((-8*av*(t[1,2]/qnorm(0.999)^2+1))^2-64*(1/qnorm(0.999)^2+1/t[1,2])*av*(t[1,2]*(av*(t[1,2]/qnorm(0.999)^2+2)-1)+qnorm(0.999)^2*
#                                                                                                                               (av-1)))/8)/(1/qnorm(0.999)^2+1/t[1,2])/t[1,2])*100)
# t[1,7]=av*100
#
#  for(j in 2:100) {
#
#   t[j,1]=t[j-1]+1
#    t[j,2]=max(round((actmaxd/t[j-1,2])^(1/(101-t[j,1]))*t[j-1,2]),t[j-1,2]+1)
#
#
#   t[j,3]= max(0,((av*(t[j,2]/qnorm(0.975)^2+1)-sqrt((-8*av*(t[j,2]/qnorm(0.975)^2+1))^2-64*(1/qnorm(0.975)^2+1/t[j,2])*av*(t[j,2]*(av*(t[j,2]/qnorm(0.975)^2+2)-1)+qnorm(0.975)^2*
#                                                                                                                             (av-1)))/8)/(1/qnorm(0.975)^2+1/t[j,2])/t[j,2])*100)
#
#   t[j,4]= min(100,((av*(t[j,2]/qnorm(0.975)^2+1)+sqrt((-8*av*(t[j,2]/qnorm(0.975)^2+1))^2-64*(1/qnorm(0.975)^2+1/t[j,2])*av*(t[j,2]*(av*(t[j,2]/qnorm(0.975)^2+2)-1)+qnorm(0.975)^2*
#                                                                                                                               (av-1)))/8)/(1/qnorm(0.975)^2+1/t[j,2])/t[j,2])*100)
# t[j,5]= max(0,((av*(t[j,2]/qnorm(0.999)^2+1)-sqrt((-8*av*(t[j,2]/qnorm(0.999)^2+1))^2-64*(1/qnorm(0.999)^2+1/t[j,2])*av*(t[j,2]*(av*(t[j,2]/qnorm(0.999)^2+2)-1)+qnorm(0.999)^2*
#                                                                                                                           (av-1)))/8)/(1/qnorm(0.999)^2+1/t[j,2])/t[j,2])*100)
#
#   t[j,6]= min(100,((av*(t[j,2]/qnorm(0.999)^2+1)+sqrt((-8*av*(t[j,2]/qnorm(0.999)^2+1))^2-64*(1/qnorm(0.999)^2+1/t[j,2])*av*(t[j,2]*(av*(t[j,2]/qnorm(0.999)^2+2)-1)+qnorm(0.999)^2*
#                                                                                                                               (av-1)))/8)/(1/qnorm(0.999)^2+1/t[j,2])/t[j,2])*100)
# t[j,7]= av*100
#}

# validate arguments
#  if (any(pull(data, {{ x }}) < 0, na.rm=TRUE)) {
#    stop("numerators must be greater than or equal to zero")
#  } else if (any(pull(data, {{ denom }}) <= 0, na.rm=TRUE)) {
#    stop("denominators must be greater than zero")
#  } else if (any(pull(data, {{ x }}) > pull(data, {{ denom }}), na.rm=TRUE)) {
#    stop("numerators must be less than or equal to denominator for a proportion statistic")
#  }
