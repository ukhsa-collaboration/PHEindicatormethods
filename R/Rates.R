# -------------------------------------------------------------------------------------------------
#' Calculates a rate with confidence limits using Byar's or Exact CI method.
#'
#' @param x the observed number(s) of events; numeric vector; no default
#' @param n the denominator (eg population-years at risk); numeric vector; no default
#' @param row_label the label to give each row of output (eg area name); character vector, no default
#' @param conf.level the required level of confidence expressed as a number between 0.9 and 1
#'                   or 90 and 100; numeric; default 0.95
#' @param multiplier the multiplier used to express the final values (eg 100,000 = rate per 100,000,
#'                   100 = percentage); numeric; default 100,000
#'
#' @return Returns a data frame of numerator, denominator, rate, lower and upper confidence limits and method
#'
#' @examples
#' phe_rate(65,100, row_label = "dummy")
#' phe_rate(65,100,99.8,100, row_label = "England - Males")
#'
#' @export
#'
#' @family phe statistical functions
#' @seealso \code{\link{phe_proportion}} for proportions,
#'          \code{\link{phe_rate}} for rates,
#'          \code{\link{phe_mean}} for means,
#'          \code{\link{phe_dsr}} for directly standardised rates,
#'          \code{\link{phe_isr}} for indirectly standardised ratios/rates and standardised mortality ratios
# -------------------------------------------------------------------------------------------------

# create function to calculate rate and CIs using Byar's method
phe_rate <- function(x, n, row_label, conf.level = 0.95, multiplier = 100000) {

  # validate arguments
  if (any(x < 0)) {
        stop("numerators must be greater than or equal to zero")
    } else if (any(n <= 0)) {
        stop("denominators must be greater than zero")
    } else if ((conf.level<0.9)|(conf.level >1 & conf.level <90)|(conf.level > 100)) {
        stop("confidence level must be between 90 and 100 or between 0.9 and 1")
    } else if (length(x) != length(n)|length(x) != length(row_label)) {
        stop("numerator, denominator and row label vectors must be of equal length")
    }

  # scale confidence level
  if (conf.level >= 90) {
    conf.level <- conf.level/100
  }

  # calculate rate and CIs
  phe_rate <- data.frame(row_label,x,n) %>%
              mutate(rate = x/n*multiplier,
              lowercl = if_else(x < 10, qchisq((1-conf.level)/2,2*x)/2/n*multiplier,
                                byars_lower(x,conf.level)/n*multiplier),
              uppercl = if_else(x < 10, qchisq(conf.level+(1-conf.level)/2,2*x+2)/2/n*multiplier,
                                byars_upper(x,conf.level)/n*multiplier),
              method  = if_else(x < 10, "Exact","Byars"))

  # set column names
  names(phe_rate) <- c("row_label","numerator","denominator","rate",
                       paste("lower",conf.level*100,"cl",sep=""),
                       paste("upper",conf.level*100,"cl",sep=""),"method")
  return(phe_rate)
}
