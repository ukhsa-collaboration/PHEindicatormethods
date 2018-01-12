# -------------------------------------------------------------------------------------------------
#' Calculates lower confidence limit for a proportion using Wilson's method.
#'
#' @param x the observed number of cases in the total sample meeting the required condition; numeric vector; no default
#' @param n the number of cases in the sample/population; numeric vector; no default
#' @param conf.level the required level of confidence expressed as a number between 0.9 and 1
#'                   or 90 and 100; numeric; default 0.95
#'
#' @return Returns a lower confidence limit for an observed number of events usign Byar's method
#'
#' @examples
#' wilson_lower(65)
#' wilson_lower(65,99.8)
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

# create function to calculate Byar's lower CI limit
wilson_lower <- function(x, n, conf.level = 0.95) {

  # validate arguments
  if (any(x < 0)) {
    stop("observed cases must all be greater than or equal to zero")
  } else if (any(n < 0)) {
      stop("sample sizes must all be greater than zero")
  } else if ((conf.level<0.9)|(conf.level >1 & conf.level <90)|(conf.level > 100)) {
      stop("confidence level must be between 90 and 100 or between 0.9 and 1")
  }


  # set z
  z <- qnorm(conf.level+(1-conf.level)/2)

  # scale confidence level
  if (conf.level >= 90) {
    conf.level <- conf.level/100
  }

  # calculate
  wilson_lower <- (2*x+z^2-z*sqrt(z^2+4*x*(1-(x/n))))/2/(n+z^2)

    return(wilson_lower)
}



# -------------------------------------------------------------------------------------------------
#' Calculates upper confidence limit for a proportion using Wilson's method.
#'
#' @param x the observed number of cases in the total sample meeting the required condition; numeric vector; no default
#' @param n the number of cases in the sample/population; numeric vector; no default
#' @param conf.level the required level of confidence expressed as a number between 0.9 and 1
#'                   or 90 and 100; numeric; default 0.95
#'
#' @return Returns a upper confidence limit for a proportion using Wilson's method
#'
#' @examples
#' wilson_upper(65)
#' wilson_upper(65,99.8)
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

# create function to calculate Byar's lower CI limit
wilson_upper <- function(x, n, conf.level = 0.95) {

  # validate arguments
  if (any(x < 0)) {
    stop("observed cases must all be greater than or equal to zero")
  } else if (any(n < 0)) {
    stop("sample sizes must all be greater than zero")
  } else if ((conf.level<0.9)|(conf.level >1 & conf.level <90)|(conf.level > 100)) {
    stop("confidence level must be between 90 and 100 or between 0.9 and 1")
  }


  # set z
  z <- qnorm(conf.level+(1-conf.level)/2)

  # scale confidence level
  if (conf.level >= 90) {
    conf.level <- conf.level/100
  }

  # calculate
  wilson_upper <- (2*x+z^2+z*sqrt(z^2+4*x*(1-(x/n))))/2/(n+z^2)

  return(wilson_upper)
}
