# -------------------------------------------------------------------------------------------------
#' na.zero
#'
#' converts NAs to zeros
#'
#' @param y input vector
#'
na.zero <- function (y) {
    y[is.na(y)] <- 0
    return(y)
}



# -------------------------------------------------------------------------------------------------
#' byars_lower
#'
#' Calculates the lower confidence limits for observed numbers of events using Byar's method [1].
#'
#' @param x the observed numbers of events; numeric vector; no default
#'
#' @inheritParams phe_dsr
#'
#' @return Returns lower confidence limits for observed numbers of events using Byar's method [1]
#'
#' @section Notes: \code{byars_lower} and \code{\link{byars_upper}} together return symmetric confidence
#'  intervals around counts, therefore
#'  for a specified confidence level, \eqn{\alpha}, the probability that, by chance, the lower limit returned
#'  will be above the true underlying value, is \eqn{\alpha}/2.
#'  If the confidence level is very close to 1 or the number of events is very small
#'  Byar's method is inaccurate and may return a negative number - in these cases an error is returned.
#'
#' @references
#' [1] Breslow NE, Day NE. Statistical methods in cancer research,
#'  volume II: The design and analysis of cohort studies. Lyon: International
#'  Agency for Research on Cancer, World Health Organisation; 1987.
#'
#' -------------------------------------------------------------------------------------------------

# create function to calculate Byar's lower CI limit
byars_lower <- function(x, confidence = 0.95) {

    # validate arguments
    if (any(x < 0, na.rm=TRUE)) {
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

    # set negative values to NA
    byars_lower[byars_lower < 0]  <- NA

    return(byars_lower)

}


# -------------------------------------------------------------------------------------------------
#' byars_upper
#'
#' Calculates the upper confidence limits for observed numbers of events using Byar's method [1].
#'
#' @param x the observed numbers of events; numeric vector; no default
#'
#' @inheritParams phe_dsr
#'
#' @return Returns upper confidence limits for observed numbers of events using Byar's method [1]
#'
#' @section Notes: \code{\link{byars_lower}} and \code{byars_upper} together return symmetric confidence
#'  intervals around counts, therefore
#'  for a specified confidence level, \eqn{\alpha}, the probability that, by chance, the upper limit returned
#'  will be below the true underlying value, is \eqn{\alpha}/2.
#'
#' @references
#' [1] Breslow NE, Day NE. Statistical methods in cancer research,
#'  volume II: The design and analysis of cohort studies. Lyon: International
#'  Agency for Research on Cancer, World Health Organisation; 1987.
#'
#' -------------------------------------------------------------------------------------------------

# create function to calculate Byar's upper CI limit
byars_upper <- function(x, confidence = 0.95) {

    # validate arguments
    if (any(x < 0, na.rm=TRUE)) {
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


# -------------------------------------------------------------------------------------------------
#' wilson_lower
#'
#' Calculates lower confidence limits for observed numbers of events using the Wilson Score method [1,2].
#'
#' @param x the observed numbers of cases in the samples meeting the required condition; numeric vector; no default
#' @param n the numbers of cases in the samples; numeric vector; no default
#'
#' @inheritParams phe_dsr
#'
#' @return Returns lower confidence limits for observed numbers of events using the Wilson Score method [1,2]
#'
#' @section Notes: \code{wilson_lower} and \code{\link{wilson_upper}} together return symmetric confidence
#'  intervals, therefore for a specified confidence level, \eqn{\alpha}, the probability that, by chance, the lower limit returned
#'  will be above the true underlying value, is \eqn{\alpha}/2.#'
#'
#' @references
#' [1] Wilson EB. Probable inference, the law of succession, and statistical
#'  inference. J Am Stat Assoc; 1927; 22. Pg 209 to 212. \cr
#' [2] Newcombe RG, Altman DG. Proportions and their differences. In Altman
#'  DG et al. (eds). Statistics with confidence (2nd edn). London: BMJ Books;
#'  2000. Pg 46 to 48.
#'
#' ------------------------------------------------------------------------------------------------

# create function to calculate Wilson's lower CI limit
wilson_lower <- function(x, n, confidence = 0.95) {

    # validate arguments
    if (any(x < 0, na.rm=TRUE)) {
        stop("observed events must all be greater than or equal to zero")
    } else if (any(n < 0, na.rm=TRUE)) {
        stop("sample sizes must all be greater than zero")
    } else if (any(x > n, na.rm=TRUE)) {
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
#'
#' @inheritParams phe_dsr
#'
#' @return Returns upper confidence limits for observed numbers of events using the Wilson Score method [1,2]
#'
#' @section Notes: \code{\link{wilson_lower}} and \code{wilson_upper} together return symmetric confidence
#'  intervals, therefore for a specified confidence level, \eqn{\alpha}, the probability that, by chance, the upper limit returned
#'  will be below the true underlying value, is \eqn{\alpha}/2.#'
#'
#' @references
#' [1] Wilson EB. Probable inference, the law of succession, and statistical
#'  inference. J Am Stat Assoc; 1927; 22. Pg 209 to 212. \cr
#' [2] Newcombe RG, Altman DG. Proportions and their differences. In Altman
#'  DG et al. (eds). Statistics with confidence (2nd edn). London: BMJ Books;
#'  2000. Pg 46 to 48.
#'
#' ------------------------------------------------------------------------------------------------

# create function to calculate Wilson's lower CI limit
wilson_upper <- function(x, n, confidence = 0.95) {

    # validate arguments
    if (any(x < 0, na.rm=TRUE)) {
        stop("observed events must all be greater than or equal to zero")
    } else if (any(n < 0, na.rm=TRUE)) {
        stop("sample sizes must all be greater than zero")
    } else if (any(x > n, na.rm=TRUE)) {
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

