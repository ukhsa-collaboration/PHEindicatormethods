# -------------------------------------------------------------------------------------------------
#' Calculates confidence interval for a mean using Normal approximation.
#'
#' @param x the observed numbers of events; numeric vector; no default
#' @param n the number of observations; numeric vector; no default
#' @param conf.level the required level of confidence expressed as a number between 0.9 and 1
#'                   or 90 and 100; numeric; default 0.95
#'
#' @return Returns a confidence interval for a mean using the Normal approximation
#'
#' @examples
#' NormMean(c(65,82,76,77,28))
#' NormMean(c(65,82,76,77,28),conf.level=99.8)
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

# create function to calculate lower confidence limit for the Normal approximation of the mean
NormMean <- function(x, n, conf.level = 0.95) {

  # validate arguments
  if (any(x < 0)) {
    stop("observed cases must all be greater than or equal to zero")
  } else if (any(n < 0)) {
    stop("sample sizes must all be greater than zero")
  } else if ((conf.level<0.9)|(conf.level >1 & conf.level <90)|(conf.level > 100)) {
    stop("confidence level must be between 90 and 100 or between 0.9 and 1")
  }

  # scale confidence level
  if (conf.level >= 90) {
    conf.level <- conf.level/100
  }

  # set p
  p <- (1 - conf.level) / 2

  # calculate
  NormMean <- abs(qt(p, numrecs - 1)) * sd((!!x)) / sqrt(numrecs)

  return(NormMean)
}
