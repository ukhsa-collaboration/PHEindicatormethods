# -------------------------------------------------------------------------------------------------
#' Calculates a rate with confidence limits using Byar's method.
#'
#' @param x the observed number(s) of events; numeric vector; no default
#' @param n the denominator (eg population-years at risk); numeric vector; no default
#' @param conf.level the required level of confidence expressed as a number between 0.9 and 1
#'                   or 90 and 100; numeric; default 0.95
#' @param multiplier the multiplier used to express the final values (eg 100,000 = rate per 100,000,
#'                   100 = percentage); numeric; default 100,000
#'
#' @return Returns a data frame of method, numerator, denominator, rate and confidence limits
#'
#' @examples
#' phe_rate(65,100)
#' phe_rate(65,100,99.8,100)
# -------------------------------------------------------------------------------------------------

# create function to calculate rate and CIs using Byar's method
phe_rate <- function(x, n, conf.level = 0.95, multiplier = 100000) {

# validate arguments
    if (any(x < 0)) {
    stop("numerators must be greater than or equal to zero")
  } else if (any(n <= 0)) {
    stop("denominators must be greater than zero")
  } else if ((conf.level<0.9)|(conf.level >1 & conf.level <90)|(conf.level > 100)) {
    stop("confidence interval must be >= 90 and <= 100 (or >= 0.9 and <= 1)")
  }

# scale confidence level
  if (conf.level >= 90) {
    conf.level <- conf.level/100
  }

# calculate rate and CIs
  rate <- x/n*multiplier
  lowercl<-byars_lower(x,conf.level)/n*multiplier
  uppercl<-byars_upper(x, conf.level)/n*multiplier

# construct output
  phe_rate <- data.frame("Byars", x, n, rate, lowercl, uppercl)
  names(phe_rate) <- c("method","numerator","denominator","rate",
                       paste("lower",conf.level*100,"cl",sep=""),
                       paste("upper",conf.level*100,"cl",sep=""))
  return(phe_rate)
}
