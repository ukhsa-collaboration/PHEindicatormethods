# -------------------------------------------------------------------------------------------------
#' Mean
#'
#' Calculates a mean with confidence limits using Student's-t distribution method.
#'
#' @param data a data.frame containing the data to calculate means for, pre-grouped if multiple means required; unquoted string; no default
#' @param x field name from data containing the values to calculate the mean for; unquoted string; no default
#'
#' @inheritParams phe_dsr
#'
#' @return When type = "full", .......r
#'
#' @importFrom rlang sym quo_name
#'
#' @examples
#' phe_mean(c(20,30,40), 0.95)
#'
#' ## Example of the grouping parameter
#' df <- data.frame(group = rep(letters[1:5], each = 5),
#'                  value = runif(25))
#' phe_mean(df$value, df$group)
#' @import dplyr
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

# create phe_proportion function to execute binom.confint with method fixed to wilson
phe_mean <- function(data, x, type = "combined", conf.level=0.95) {

  # check required arguments present
  if (missing(data)) {
    stop("data must contain a data.frame object")
  } else if (missing(x)) {
    stop("x must contain an unquoted field name from data")
  }

  # apply quotes
  x <- enquo(x)

  # validate arguments - copied from proportion need editing
  if ((conf.level<0.9)|(conf.level >1 & conf.level <90)|(conf.level > 100)) {
    stop("confidence level must be between 90 and 100 or between 0.9 and 1")
  } else if (!(type %in% c("value", "lower", "upper", "combined", "full"))) {
    stop("type must be one of value, lower, upper, combined or full")
  }

  # scale confidence level
  if (conf.level >= 90) {
    conf.level <- conf.level/100
  }

#  p <- (1 - conf.level) / 2

  # calculate proportion and CIs
  phe_mean <- data %>%
       summarise(sum_values   = sum(!!x),
                 count_values = length(!!x),
                 stdev   = sd(!!x)) %>%
       mutate(mean = total / numrecs,
              lowercl = mean - abs(qt(p, numrecs - 1)) * stdev / sqrt(numrecs),
              uppercl = mean + abs(qt(p, numrecs - 1)) * stdev / sqrt(numrecs),
#              lowercl = mean - NormMean((!!x),(!!n),conf.level),
#              uppercl = mean + NormMean((!!x),(!!n),conf.level),
              confidence = paste(conf.level*100,"%"),
              method  = "Student's t-distribution")


  if (type == "lower") {
    phe_rate <- phe_rate %>%
      select(-sum_values, -count_values, -stdev, -mean, -uppercl, -confidence, -method)
  } else if (type == "upper") {
    phe_rate <- phe_rate %>%
      select(-sum_values, -count_values, -stdev, -mean, -lowercl, -confidence, -method)
  } else if (type == "value") {
    phe_rate<- phe_rate %>%
      select(-sum_values, -count_values, -stdev, -lowercl, -uppercl, -confidence, -method)
  } else if (type == "combined") {
    phe_rate <- phe_rate %>%
      select(-sum_values, -count_values, -stdev, -confidence, -method)
  }


  return(phe_mean)
}
