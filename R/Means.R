# -------------------------------------------------------------------------------------------------
#' phe_mean
#'
#' Calculates a mean with confidence limits using Student's t-distribution method.
#'
#' @param data a data.frame containing the data to calculate means for, pre-grouped if multiple means required; unquoted string; no default
#' @param x field name from data containing the values to calculate the means for; unquoted string; no default
#'
#' @inheritParams phe_dsr
#'
#' @return When type = "full", returns a data.frame of value_sum, value_count, stdev, value, lowercl, uppercl, confidence, statistic and method
#'         for each grouping set
#'
#' @importFrom rlang sym quo_name
#'
#' @examples
#' df <- data.frame(values = c(30,40,50,60))
#' phe_mean(df, values)
#'
#' OR
#'
#' df2 <- data.frame(area = rep(c("Area1", "Area2"),each=3),
#'                   values = c(20,30,40,200,300,400)) %>%
#'                   group_by(area)
#' phe_mean(df2,values)
#' phe_mean(df2,values,type="full", confidence=0.998)
#'
#'
#' @import dplyr
#'
#' @export
#'
#' @family PHEstatmethods package functions
# -------------------------------------------------------------------------------------------------

# create phe_mean function using Student's t-distribution method
phe_mean <- function(data, x, type = "standard", confidence=0.95) {

  # check required arguments present
  if (missing(data)|missing(x)) {
    stop("function phe_dsr requires at least 2 arguments: data, x")
  }

  # apply quotes
  x <- enquo(x)

  # validate arguments - copied from proportion need editing
  if ((confidence<0.9)|(confidence >1 & confidence <90)|(confidence > 100)) {
    stop("confidence level must be between 90 and 100 or between 0.9 and 1")
  } else if (!(type %in% c("value", "lower", "upper", "standard", "full"))) {
    stop("type must be one of value, lower, upper, standard or full")
  }

  # scale confidence level
  if (confidence >= 90) {
    confidence <- confidence/100
  }

  p <- (1 - confidence) / 2

  # calculate proportion and CIs
  phe_mean <- data %>%
       summarise(value_sum   = sum(!!x),
                 value_count = length(!!x),
                 stdev   = sd(!!x)) %>%
       mutate(value = value_sum / value_count,
              lowercl = value - abs(qt(p, value_count - 1)) * stdev / sqrt(value_count),
              uppercl = value + abs(qt(p, value_count - 1)) * stdev / sqrt(value_count),
              confidence = paste(confidence*100,"%",sep=""),
              statistic = "mean",
              method  = "Student's t-distribution")


  if (type == "lower") {
    phe_mean <- phe_mean %>%
      select(-value_sum, -value_count, -stdev, -value, -uppercl, -confidence, -statistic, -method)
  } else if (type == "upper") {
    phe_mean <- phe_mean %>%
      select(-value_sum, -value_count, -stdev, -value, -lowercl, -confidence, -statistic, -method)
  } else if (type == "value") {
    phe_mean <- phe_mean %>%
      select(-value_sum, -value_count, -stdev, -lowercl, -uppercl, -confidence, -statistic, -method)
  } else if (type == "standard") {
    phe_mean <- phe_mean %>%
      select(-value_sum, -value_count, -stdev, -confidence, -statistic, -method)
  }


  return(phe_mean)
}
