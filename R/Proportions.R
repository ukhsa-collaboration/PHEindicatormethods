# -------------------------------------------------------------------------------------------------
#' Proportion
#'
#' Calculates a proportion with confidence limits using Wilson method.
#'
#' @param data a data.frame containing the data to calculate proportions for; unquoted string; no default
#' @param x field name from data containing the observed numbers of cases in the sample meeting the required condition; unquoted string; no default
#' @param n field name from data containing the number of cases in the sample;
#'          unquoted string; no default
#' @param percentage whether the output should be returned as a percentage; logical; default FALSE
#'
#' @inheritParams phe_dsr
#'
#' @return When type=full, returns the original data.frame with the following columns appended:
#'         proportion, lower confidence limit, upper confidence limit, confidence level, statistic and method
#'
#' @importFrom rlang sym quo_name
#'
#' @examples
#' df <- data.frame(area = c("Area1","Area2","Area3"), numerator = c(65,82,100), denominator = c(100,100,100))
#' phe_proportion(df, numerator, denominator)
#' phe_proportion(df, numerator, denominator, conf.level=99.8)
#' phe_proportion(df, numerator, denominator, type="full")
#'
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
phe_proportion <- function(data, x, n, type="standard", confidence=0.95, percentage=FALSE) {

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
    } else if (any(pull(data, !!x) > pull(data, !!n))) {
        stop("numerators must be less than or equal to denominator for a proportion statistic")
    } else if ((confidence<0.9)|(confidence >1 & confidence <90)|(confidence > 100)) {
        stop("confidence level must be between 90 and 100 or between 0.9 and 1")
    } else if (!(type %in% c("value", "lower", "upper", "standard", "full"))) {
      stop("type must be one of value, lower, upper, combined or full")
    }

  # scale confidence level
  if (confidence >= 90) {
    confidence <- confidence/100
  }

  # set multiplier
  multiplier <- 1
  if (percentage == TRUE) {
    multiplier <- 100
  }

  # calculate proportion and CIs
  phe_proportion <- data %>%
                    mutate(proportion = (!!x)/(!!n) * multiplier,
                           lowercl = wilson_lower((!!x),(!!n),conf.level) * multiplier,
                           uppercl = wilson_upper((!!x),(!!n),conf.level) * multiplier,
                           confidence = paste(confidence*100,"%",sep=""),
                           statistic = if_else(percentage == TRUE,'Percentage','Proportion'),
                           method = "Wilson")

  if (type == "lower") {
    phe_proportion <- phe_proportion %>%
      select(-proportion, -uppercl, -confidence, -statistic, -method)
  } else if (type == "upper") {
    phe_proportion <- phe_proportion %>%
      select(-proportion, -lowercl, -confidence, -statistic, -method)
  } else if (type == "value") {
    phe_proportion<- phe_proportion %>%
      select(-lowercl, -uppercl, -confidence, -statistic, -method)
  } else if (type == "standard") {
    phe_proportion <- phe_proportion %>%
      select( -confidence, -statistic, -method)
  }

return(phe_proportion)
}
