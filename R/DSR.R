# define the European Standard Population
esp2013 <- c(5000,5500,5500,5500,6000,6000,6500,7000,7000,
             7000,7000,6500,6000,5500,5000,4000,2500,1500,1000)

##test data - remove later)
testpop <- c(84935,80367,72122,79259,99806,87362,81579,71103,
             70001,69007,63203,52638,46087,40887,32604,28399,
             21625,13021,7355)
testobs <- c(27,45,55,100,125,300,295,270,275,450,455,459,345,300,
             270,265,100,90,35)



# -------------------------------------------------------------------------------------------------
#' Calculates a directly standardised rate with confidence limits using Dobson method.
#'
#' @param x the observed number of events for each standardisation category (eg age band);
#'          numeric vector; no default
#' @param n the populations for each standardisation category (eg age band);
#'          numeric vector; no default
#' @param stdpop the standard populations for each standardisation category
#'               (eg age band); numeric vector; default is the European Standard Population 2013
#'              with 19 five-year age band categories
#' @param conf.level the required level of confidence expressed as a number between 0.9 and 1
#'                   or 90 and 100; numeric; default 0.95
#' @param multiplier the multiplier used to express the final values (eg 100,000 = rate per 100,000,
#'                   100 = percentage); numeric; default 100,000
#'
#' @return Returns a data frame of method, numerator, denominator, directly standardised rate
#'         and confidence interval limits
#'
#' @examples
#' phe_dsr(c(27,45,55,100,125,300,295,270,275,450,455,459,345,300,270,265,100,90,35),
#'         c(84935,80367,72122,79259,99806,87362,81579,71103,70001,
#'           69007,63203,52638,46087,40887,32604,28399,21625,13021,7355),conf.level = 0.998)
# -------------------------------------------------------------------------------------------------

# define the DSR function
phe_dsr <- function(x,n,stdpop = esp2013, conf.level = 0.95, multiplier = 100000) {

# validate arguments
  if (any(x < 0)) {
    stop("numerators must all be greater than or equal to zero")
  } else if (any(n <= 0)) {
    stop("denominators must all be greater than zero")
  } else if ((conf.level<0.9)|(conf.level >1 & conf.level <90)|(conf.level > 100)) {
    stop("confidence level must be >= 90 and <= 100 (or >= 0.9 and <= 1)")
  } else if (length(x) != length(n)) {
    stop("numerator and denominator vectors must be of equal length")
  } else if (length(x) %% length(stdpop) !=0) {
    stop("numerator vector length must be a multiple of standard population vector length")
  } else if (sum(x) < 10) {
    stop("DSR calculation is not valid for total counts < 10")
  }

# scale confidence level
  if (conf.level >= 90) {
    conf.level <- conf.level/100
  }

# Calculate DSR
dsr <- sum(x * stdpop / n) / sum(stdpop) * multiplier

# Calculate CIs using Byars function created in Rates.R
vardsr  <- 1/sum(stdpop)^2 * sum((stdpop^2 * x) / n^2)
lowercl <- dsr + sqrt((vardsr/sum(x)))*(byars_lower(sum(x),conf.level)-sum(x)) * multiplier
uppercl <- dsr + sqrt((vardsr/sum(x)))*(byars_upper(sum(x),conf.level)-sum(x)) * multiplier


phe_dsr <- data.frame("Dobson",sum(x), sum(n), dsr, lowercl, uppercl)
names(phe_dsr) <- c("method","sum(numerator)","sum(denominator)","rate",
                    paste("lower",conf.level*100,"cl",sep=""),
                    paste("upper",conf.level*100,"cl",sep=""))
return(phe_dsr)

}
