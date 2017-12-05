# -------------------------------------------------------------------------------------------------
#' Calculates lower confidence limit for an observed number of events using Byar's method.
#'
#' @param x the observed number of events; numeric; no default
#' @param conf.level the required level of confidence expressed as a number between 0.9 and 1
#'                   or 90 and 100; numeric; default 0.95
#'
#' @return Returns a lower confidence limit
#'
#' @examples
#' byars_lower(65)
#' byars_lower(65,99.8)
# -------------------------------------------------------------------------------------------------

# create function to calculate Byar's lower CI limit
byars_lower <- function(x, conf.level = 0.95) {

  # validate arguments
  if (any(x < 0)) {
    stop("observed events (x) must all be greater than or equal to zero")
  } else if ((conf.level<0.9)|(conf.level >1 & conf.level <90)|(conf.level > 100)) {
    stop("confidence interval must be >= 90 and <= 100 (or >= 0.9 and <= 1)")
  }


    # scale confidence level
    if (conf.level >= 90) {
      conf.level <- conf.level/100
    }

  byars_lower <- x*(1-1/(9*x)-qnorm(conf.level/2)/(3*sqrt(x)))^3
  return(byars_lower)
}



# -------------------------------------------------------------------------------------------------
#' Calculates upper confidence limit for an observed number of events using Byar's method.
#'
#' @param x the observed number of events; numeric; no default
#' @param conf.level the level of confidence required expressed as a number between 0.9 and 1
#'                   or 90 and 100; numeric; default 0.95
#'
#'
#' @return Returns an upper confidence limit
#'
#' @examples
#' byars_upper(65)
#' byars_upper(65,99.8)
# -------------------------------------------------------------------------------------------------

# create function to calculate Byar's upper CI limit
byars_upper <- function(x, conf.level = 0.95) {

  # validate arguments
  if (any(x < 0)) {
    stop("observed events (x) must all be greater than or equal to zero")
  } else if ((conf.level<0.9)|(conf.level >1 & conf.level <90)|(conf.level > 100)) {
    stop("confidence interval must be >= 90 and <= 100 (or >= 0.9 and <= 1)")
  }


    # scale confidence level
    if (conf.level >= 90) {
      conf.level <- conf.level/100
    }

  byars_upper <- (x+1)*(1-1/(9*(x+1))+qnorm(conf.level/2)/(3*sqrt(x+1)))^3
  return(byars_upper)
}


