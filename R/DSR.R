# Need to add preamble to generate help files

# create phe_dasr function to calculate a rate from numerator and denominator
# with optional multiplier and CI inputs.  Dobson method = byars CI method for all numerators or exact below 389 or 100 ?



# see https://cran.r-project.org/web/packages/exactci/exactci.pdf
# see https://cran.r-project.org/web/packages/ratesci/index.html
# see https://cran.r-project.org/web/packages/epitools/epitools.pdf
# see https://www.rdocumentation.org/packages/epitools/versions/0.5-7/topics/ageadjust.direct
# ems package has SMR function shere can specify Byars Ci method


# ageadjust.direct(DSR_testdata$count, DSR_testdata$pop, rate = NULL, DSR_testdata$stdpop, conf.level = 0.95)
# ?? surely rate = NULL not required as the age specific rates are determined by o and n ??

#__________________________________________________________________________________________________
# Need to add preamble to generate help files

#__________________________________________________________________________________________________


#Define European Standard Population
esp2013 <- data.frame(ageband=c("0-4",  "5-9",  "10-14", "15-19","20-24","25-29","30-34","35-39","40-44",
                          "45-49","50-54","55-69","60-64","65-69","70-74","75-79","80-84","85-89","90+"),
                      stdpop=c(5000,5500,5500,5500,6000,6000,6500,7000,7000,7000,7000,6500,6000,
                          5500,5000,4000,2500,1500,1000))
#__________________________________________________________________________________________________

# create phe_proportion function to execute binom.confint with method fixed to wilson
phe_dasr <- function(O, n, rate = NULL, stdpop = esp2013, conf.level=0.95, multiplier=100000) {
#  if (x < 0) {
#    stop("numerator must be greater than or equal to zero")
#  } else if (n <= 0) {
#    stop("denominator must be greater than zero")
#  } else if (x > n) {
#    stop("numerator must be less than or equal to denominator for a proportion statistic")
#  } else if (!(max %in% c(1,100))) {
#    stop("function phe_proportion can only output a proportion (max=1) or a percentage (max=100)")
#  }

# stop if count <10

    phe_dasr <- ageadjust.direct(O, n, rate = NULL, stdpop = esp2013$stdpop, conf.level = 0.95) %>%
       select(x,n,mean,lower,upper) * c(1,1,max,max,max) %>%
       mutate(method="Dobson")
    # define correct CI separately if this not accurate enough - Dobson method = byars for O >= 100 or exact method below?
    # phe_dasr * multiplier %>%
    return(phe_dasr)
}


# testdata small - crude rate (no multiplier) 0.010144928, adj rate (100000 multipler) 1103.2120,
# LL 95 CI 877.4812, UL 95 CI 1367.0732
# dependencies for calculating LL 95 Ci - adj.rate, sum(O), sum(stdpop), multiplier, CI level,




