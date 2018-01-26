# -------------------------------------------------------------------------------------------------
#' Rate
#'
#' Calculates a rate with confidence limits using Byar's or Exact CI method.
#'
#' @param data the data.frame containing the data to calculate rates for; unquoted string; no default
#' @param x field name from data containing the rate numerators (eg observed number of events); unquoted string; no default
#' @param n field name from data containing the rate denominators (eg populations); unquoted string; no default
#'
#' @inheritParams phe_dsr
#'
#' @return When type=full, returns the original data.frame with the following columns appended:
#'         rate, lower confidence limit, upper confidence limit, confidence level, statistic and method
#'
#' @importFrom rlang sym quo_name
#'
#' @import dplyr
#'
#' @examples
#' library(dplyr)
#' df <- data.frame(area = rep(c("Area1","Area2","Area3","Area4"), 2),
#'                  year = rep(2015:2016, each = 4),
#'                  obs = sample(100, 2 * 4, replace = TRUE),
#'                  pop = sample(100:200, 2 * 4, replace = TRUE))
#' phe_rate(df, obs, pop)
#' phe_rate(df, obs, pop, type="full", confidence=99.8, multiplier=100)
#'
#' @export
#'
#' @family phe statistical functions
# -------------------------------------------------------------------------------------------------

# create function to calculate rate and CIs using Byar's method
phe_rate <- function(data,x, n, type = "standard", confidence = 0.95, multiplier = 100000) {

    # check required arguments present
  if (missing(data)|missing(x)|missing(n)) {
    stop("function phe_dsr requires at least 3 arguments: data, x, n")
  }


  # apply quotes
  x <- enquo(x)
  n <- enquo(n)

  # validate arguments
  if (any(pull(data, !!x) < 0)) {
        stop("numerators must be greater than or equal to zero")
    } else if (any(pull(data, !!n) <= 0)) {
        stop("denominators must be greater than zero")
    } else if ((confidence<0.9)|(confidence >1 & confidence <90)|(confidence > 100)) {
        stop("confidence level must be between 90 and 100 or between 0.9 and 1")
    } else if (!(type %in% c("value", "lower", "upper", "standard", "full"))) {
      stop("type must be one of value, lower, upper, standard or full")
    }

  # scale confidence level
  if (confidence >= 90) {
    confidence <- confidence/100
  }

  # calculate rate and CIs
  phe_rate <- data %>%
              mutate(rate = (!!x)/(!!n)*multiplier,
              lowercl = if_else((!!x) < 10, qchisq((1-confidence)/2,2*(!!x))/2/(!!n)*multiplier,
                                byars_lower((!!x),confidence)/(!!n)*multiplier),
              uppercl = if_else((!!x) < 10, qchisq(confidence+(1-confidence)/2,2*(!!x)+2)/2/(!!n)*multiplier,
                                byars_upper((!!x),confidence)/(!!n)*multiplier),
              confidence = paste(confidence*100,"%",sep=""),
              statistic = paste("rate per",as.character(format(multiplier, scientific=F))),
              method  = if_else((!!x) < 10, "Exact","Byars"))

  if (type == "lower") {
    phe_rate <- phe_rate %>%
      select(-rate, -uppercl, -confidence, -statistic, -method)
  } else if (type == "upper") {
    phe_rate <- phe_rate %>%
      select(-rate, -lowercl, -confidence, -statistic, -method)
  } else if (type == "value") {
    phe_rate<- phe_rate %>%
      select(-lowercl, -uppercl, -confidence, -statistic, -method)
  } else if (type == "standard") {
    phe_rate <- phe_rate %>%
      select( -confidence, -statistic, -method)
  }

  return(phe_rate)
}
