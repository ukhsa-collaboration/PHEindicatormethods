# -------------------------------------------------------------------------------------------------
#' wilson_lower
#'
#' Calculates lower confidence limit for an observed number of events using Wilson's method.
#'
#' @param x the observed number of cases in the sample meeting the required condition; numeric vector; no default
#' @param n the number of cases in the sample; numeric vector; no default
#' @inheritParams phe_dsr
#'
#' @return Returns a lower confidence limit for an observed number of events using Wilson's method
#'
#' @examples
#' wilson_lower(65,100)
#' wilson_lower(65,100,99.8)
#'
#' @export
#'
#' @family PHEstatmethods package functions
# -------------------------------------------------------------------------------------------------

# create function to calculate Wilson's lower CI limit
wilson_lower <- function(x, n, confidence = 0.95) {

  # validate arguments
  if (any(x < 0)) {
    stop("observed cases must all be greater than or equal to zero")
  } else if (any(n < 0)) {
      stop("sample sizes must all be greater than zero")
  } else if ((confidence<0.9)|(confidence >1 & confidence <90)|(confidence > 100)) {
      stop("confidence level must be between 90 and 100 or between 0.9 and 1")
  }

  # scale confidence level
  if (confidence >= 90) {
    confidence <- confidence/100
  }

  # set z
  z <- qnorm(confidence+(1-confidence)/2)

  # calculate
  wilson_lower <- (2*x+z^2-z*sqrt(z^2+4*x*(1-(x/n))))/2/(n+z^2)

  return(wilson_lower)
}



# -------------------------------------------------------------------------------------------------
#' wilson_upper
#'
#' Calculates upper confidence limit for an observed number of events using Wilson's method.
#'
#' @param x the observed number of cases in the sample meeting the required condition; numeric vector; no default
#' @param n the number of cases in the sample; numeric vector; no default
#' @inheritParams phe_dsr
#'
#' @return Returns an upper confidence limit for an observed number of events using Wilson's method
#'
#' @examples
#' wilson_upper(65)
#' wilson_upper(65,99.8)
#'
#' @export
#'
#' @family PHEstatmethods package functions
# -------------------------------------------------------------------------------------------------

# create function to calculate Wilson's lower CI limit
wilson_upper <- function(x, n, confidence = 0.95) {

  # validate arguments
  if (any(x < 0)) {
    stop("observed cases must all be greater than or equal to zero")
  } else if (any(n < 0)) {
    stop("sample sizes must all be greater than zero")
  } else if ((confidence<0.9)|(confidence >1 & confidence <90)|(confidence > 100)) {
    stop("confidence level must be between 90 and 100 or between 0.9 and 1")
  }

  # scale confidence level
  if (confidence >= 90) {
    confidence <- confidence/100
  }

  # set z
  z <- qnorm(confidence+(1-confidence)/2)

  # calculate
  wilson_upper <- (2*x+z^2+z*sqrt(z^2+4*x*(1-(x/n))))/2/(n+z^2)

  return(wilson_upper)
}


zscore <- function(num) {
  qnorm(num+(1-num)/2)
}

