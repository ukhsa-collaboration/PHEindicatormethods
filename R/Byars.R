# -------------------------------------------------------------------------------------------------
#' byars_lower
#'
#' Calculates the lower confidence limits for observed numbers of events using Byar's method.
#'
#' @param x the observed numbers of events; numeric vector; no default
#' @inheritParams phe_dsr
#'
#' @return Returns lower confidence limits for observed numbers of events using Byar's method
#'
#' @section Notes: if the confidence level is very close to 1 or the number of events is very small
#' Byar's method is inaccurate and may return a negative number - in these cases an error is returned.
#'
#' @examples
#' byars_lower(65)
#' byars_lower(65,99.8)
#'
#' @export
#'
#' @family PHEindicatormethods package functions
# -------------------------------------------------------------------------------------------------

# create function to calculate Byar's lower CI limit
byars_lower <- function(x, confidence = 0.95) {

  # validate arguments
  if (any(x < 0)) {
        stop("observed events must all be greater than or equal to zero")
    } else if ((confidence<0.9)|(confidence >1 & confidence <90)|(confidence > 100)) {
        stop("confidence level must be between 90 and 100 or between 0.9 and 1")
    }

  # scale confidence level
    if (confidence >= 90) {
      confidence <- confidence/100
    }

  # populate z
  z <- qnorm(confidence + (1-confidence)/2)

  # calculate
  byars_lower <- x*(1-1/(9*x)-z/(3*sqrt(x)))^3

 # if(any(byars_lower < 0)) {
#    warning("some lower confidence limits could not be calculated due to small numerators or high confidence levels")
#  }

  byars_lower[byars_lower < 0]  <- NA

  return(byars_lower)

}


# -------------------------------------------------------------------------------------------------
#' byars_upper
#'
#' Calculates the upper confidence limits for observed numbers of events using Byar's method.
#'
#' @param x the observed numbers of events; numeric vector; no default
#' @inheritParams phe_dsr
#'
#' @return Returns upper confidence limits for observed numbers of events using Byar's method
#'
#' @section Notes: if the confidence level is close to 1 (or 100) or the number of events is very small
#' Byar's method is inaccurate and could return a negative number - in these cases an error is returned.
#'
#' @examples
#' byars_upper(65)
#' byars_upper(65,99.8)
#'
#' @export
#'
#' @family PHEindicatormethods package functions
# -------------------------------------------------------------------------------------------------

# create function to calculate Byar's upper CI limit
byars_upper <- function(x, confidence = 0.95) {

  # validate arguments
  if (any(x < 0)) {
      stop("observed events must all be greater than or equal to zero")
  } else if ((confidence<0.9)|(confidence >1 & confidence <90)|(confidence > 100)) {
      stop("confidence level must be between 90 and 100 or between 0.9 and 1")
  }

  # scale confidence level
  if (confidence >= 90) {
      confidence <- confidence/100
  }

  # populate z
  z <- qnorm(confidence + (1-confidence)/2)

  byars_upper <- (x+1)*(1-1/(9*(x+1))+z/(3*sqrt(x+1)))^3
  return(byars_upper)
}


