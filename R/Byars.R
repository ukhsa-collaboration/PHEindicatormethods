# -------------------------------------------------------------------------------------------------
#' byars_lower
#'
#' Calculates the lower confidence limit for an observed number of events using Byar's method.
#'
#' @param x the observed number of events; numeric vector; no default
#' @param confidence the required level of confidence expressed as a number between 0.9 and 1
#'                   or 90 and 100; numeric; default 0.95
#'
#' @return Returns a lower confidence limit for an observed number of events using Byar's method
#'
#' @examples
#' byars_lower(65)
#' byars_lower(65,99.8)
#'
#' @export
#'
#' @family PHEstatmethods package functions
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
  return(byars_lower)
}


# -------------------------------------------------------------------------------------------------
#' byars_upper
#'
#' Calculates the upper confidence limit for an observed number of events using Byar's method.
#'
#' @param x the observed number of events; numeric vector; no default
#' @param confidence the level of confidence required expressed as a number between 0.9 and 1
#'                   or 90 and 100; numeric; default 0.95
#'
#' @return Returns an upper confidence limit for an observed number of events using Byar's method
#'
#' @examples
#' byars_upper(65)
#' byars_upper(65,99.8)
#'
#' @export
#'
#' @family PHEstatmethods package functions
# -------------------------------------------------------------------------------------------------

# create function to calculate Byar's upper CI limit
byars_upper <- function(x, confidence = 0.95) {

  # validate arguments
  if (any(x < 0)) {
      stop("observed events (x) must all be greater than or equal to zero")
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


