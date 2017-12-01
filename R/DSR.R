# Need to add preamble to generate help files

# define the European Standard Population
esp2013 <- c(5000,5500,5500,5500,6000,6000,6500,7000,7000,7000,7000,6500,6000,
                               5500,5000,4000,2500,1500,1000)
##test data - remove later)
testpop <- c(84935,80367,72122,79259,99806,87362,81579,71103,
             70001,69007,63203,52638,46087,40887,32604,28399,
             21625,13021,7355)
testobs <- c(27,45,55,100,125,300,295,270,275,450,455,459,345,300,
             270,265,100,90,35)


# define the function
phe_dsr <- function(x,n,stdpop = esp2013, conf.level = 0.95, multiplier = 100000) {

# Calculate DSR
dsr <- sum(x * stdpop / n) / sum(stdpop) * multiplier

# Calculate CIs using Byars function created in Rates.R
vardsr<-1/sum(stdpop)^2 * sum((stdpop^2 * x) / n^2)
vardsr
lowercl<- dsr + sqrt((vardsr/sum(x)))*(byars_lower(sum(x))-sum(x)) * multiplier
lowercl
uppercl<- dsr + sqrt((vardsr/sum(x)))*(byars_upper(sum(x))-sum(x)) * multiplier
uppercl

phe_dsr <- data.frame("Dobson",sum(x), sum(n), dsr, lowercl, uppercl)
names(phe_dsr) <- c("method","sum(numerator)","sum(denominator)","rate",paste("lower",conf.level*100,"cl",sep=""),paste("upper",conf.level*100,"cl",sep=""))
return(phe_dsr)

}
