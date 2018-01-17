# -------------------------------------------------------------------------------------------------
#' Calculates a rate with confidence limits using Byar's or Exact CI method.
#'
#' @param data the data.frame containing the data to calculate rates for
#' @param x field name from data containing the rate numerators (eg observed number of events); unquoted string; no default
#' @param n field name from data containing the rate denominators (eg populations); unquoted string; no default
#'
#' @inheritParams phe_dsr
#'
#' @return When type = "full" - adds rate, lower confidence limit, upper confidence limit, confidence level and method to the original data.frame
#'
#' @importFrom rlang sym quo_name
#'
#' @examples
#' library(dplyr)
#' df <- data.frame(area = rep(c("Area1","Area2","Area3","Area4"), 2),
#'                  year = rep(2015:2016, each = 4),
#'                  obs = sample(100, 2 * 4, replace = TRUE),
#'                  pop = sample(100:200, 2 * 4, replace = TRUE))
#' phe_rate(df, obs, pop)
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
phe_rate <- function(data,x, n, type = "combined", conf.level = 0.95, multiplier = 100000) {

    # check required arguments present
  if (missing(data)) {
    stop("data must contain a data.frame object")
  } else if (missing(x)) {
    stop("x must contain an unquoted field name from data")
  } else if (missing(n)) {
    stop("n must contain an unquoted field name from data")
  }

  # apply quotes
  x <- enquo(x)
  n <- enquo(n)

  # validate arguments
  if (any(pull(data, !!x) < 0)) {
        stop("numerators must be greater than or equal to zero")
    } else if (any(pull(data, !!n) <= 0)) {
        stop("denominators must be greater than zero")
    } else if ((conf.level<0.9)|(conf.level >1 & conf.level <90)|(conf.level > 100)) {
        stop("confidence level must be between 90 and 100 or between 0.9 and 1")
    } else if (!(type %in% c("value", "lower", "upper", "combined", "full"))) {
      stop("type must be one of value, lower, upper, combined or full")
    }

  # scale confidence level
  if (conf.level >= 90) {
    conf.level <- conf.level/100
  }

  # calculate rate and CIs
  phe_rate <- data %>%
              mutate(rate = (!!x)/(!!n)*multiplier,
              lowercl = if_else((!!x) < 10, qchisq((1-conf.level)/2,2*(!!x))/2/(!!n)*multiplier,
                                byars_lower((!!x),conf.level)/(!!n)*multiplier),
              uppercl = if_else((!!x) < 10, qchisq(conf.level+(1-conf.level)/2,2*(!!x)+2)/2/(!!n)*multiplier,
                                byars_upper((!!x),conf.level)/(!!n)*multiplier),
              confidence = paste(conf.level*100,"%"),
              method  = if_else((!!x) < 10, "Exact","Byars"))

  if (type == "lower") {
    phe_rate <- phe_rate %>%
      select(-rate, -uppercl, -confidence, -method)
  } else if (type == "upper") {
    phe_rate <- phe_rate %>%
      select(-rate, -lowercl, -confidence, -method)
  } else if (type == "value") {
    phe_rate<- phe_rate %>%
      select(-lowercl, -uppercl, -confidence, -method)
  } else if (type == "combined") {
    phe_rate <- phe_rate %>%
      select( -confidence, -method)
  }

  return(phe_rate)
}
