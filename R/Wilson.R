# -------------------------------------------------------------------------------------------------
#' wilson_lower
#'
#' Calculates lower confidence limits for observed numbers of events using the Wilson Score method [1,2].
#'
#' @param x the observed numbers of cases in the samples meeting the required condition; numeric vector; no default
#' @param n the numbers of cases in the samples; numeric vector; no default
#' @inheritParams phe_dsr
#'
#' @return Returns lower confidence limits for observed numbers of events using the Wilson Score method [1,2]
#'
#' @section Notes: \code{wilson_lower} and \code{\link{wilson_upper}} together return symmetric confidence
#'  intervals, therefore for a specified confidence level, \eqn{\alpha}, the probability that, by chance, the lower limit returned
#'  will be above the true underlying value, is \eqn{\alpha}/2.#'
#'
#' @examples
#' wilson_lower(65,100)
#' wilson_lower(65,100,99.8)
#'
#' @references
#' [1] Wilson EB. Probable inference, the law of succession, and statistical
#'  inference. J Am Stat Assoc; 1927; 22. Pg 209–12. \cr
#' [2] Newcombe RG, Altman DG. Proportions and their differences. In Altman
#'  DG et al. (eds). Statistics with confidence (2nd edn). London: BMJ Books;
#'  2000. Pg 46–8.
#'
#' @export
#'
#' @family PHEindicatormethods package functions
# -------------------------------------------------------------------------------------------------

# create function to calculate Wilson's lower CI limit
wilson_lower <- function(x, n, confidence = 0.95) {

  # validate arguments
  if (any(x < 0)) {
    stop("observed events must all be greater than or equal to zero")
  } else if (any(n < 0)) {
      stop("sample sizes must all be greater than zero")
  } else if (any(x > n)) {
      stop("numerators must be less than or equal to denominator for a Wilson score to be calculated")
  } else if ((confidence<0.9)|(confidence >1 & confidence <90)|(confidence > 100)) {
      stop("confidence level must be between 90 and 100 or between 0.9 and 1")
  }

  # scale confidence level
  if (confidence >= 90) {
    confidence <- confidence/100
  }

  if (confidence == 1) {
    wilson_lower <- 0
  } else {

    # set z
    z <- qnorm(confidence+(1-confidence)/2)

    # calculate
    wilson_lower <- (2*x+z^2-z*sqrt(z^2+4*x*(1-(x/n))))/2/(n+z^2)

  }

  return(wilson_lower)
}



# -------------------------------------------------------------------------------------------------
#' wilson_upper
#'
#' Calculates upper confidence limits for observed numbers of events using the Wilson Score method [1,2].
#'
#' @param x the observed numbers of cases in the samples meeting the required condition; numeric vector; no default
#' @param n the numbers of cases in the samples; numeric vector; no default
#' @inheritParams phe_dsr
#'
#' @return Returns upper confidence limits for observed numbers of events using the Wilson Score method [1,2]
#'
#' @section Notes: \code{\link{wilson_lower}} and \code{wilson_upper} together return symmetric confidence
#'  intervals, therefore for a specified confidence level, \eqn{\alpha}, the probability that, by chance, the upper limit returned
#'  will be below the true underlying value, is \eqn{\alpha}/2.#'
#'
#' @examples
#' wilson_upper(65,100)
#' wilson_upper(65,100,99.8)
#'
#' @references
#' [1] Wilson EB. Probable inference, the law of succession, and statistical
#'  inference. J Am Stat Assoc; 1927; 22. Pg 209–12. \cr
#' [2] Newcombe RG, Altman DG. Proportions and their differences. In Altman
#'  DG et al. (eds). Statistics with confidence (2nd edn). London: BMJ Books;
#'  2000. Pg 46–8.
#'
#' @export
#'
#' @family PHEindicatormethods package functions
# -------------------------------------------------------------------------------------------------

# create function to calculate Wilson's lower CI limit
wilson_upper <- function(x, n, confidence = 0.95) {

  # validate arguments
  if (any(x < 0)) {
      stop("observed events must all be greater than or equal to zero")
  } else if (any(n < 0)) {
      stop("sample sizes must all be greater than zero")
  } else if (any(x > n)) {
      stop("numerators must be less than or equal to denominator for a Wilson score to be calculated")
  } else if ((confidence<0.9)|(confidence >1 & confidence <90)|(confidence > 100)) {
      stop("confidence level must be between 90 and 100 or between 0.9 and 1")
  }

  # scale confidence level
  if (confidence >= 90) {
    confidence <- confidence/100
  }

  if (confidence == 1) {
    wilson_upper <- 1
  } else {

    # set z
    z <- qnorm(confidence+(1-confidence)/2)

    # calculate
    wilson_upper <- (2*x+z^2+z*sqrt(z^2+4*x*(1-(x/n))))/2/(n+z^2)

  }
  return(wilson_upper)
}

