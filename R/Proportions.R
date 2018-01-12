# -------------------------------------------------------------------------------------------------
#' Calculates a proportion with confidence limits using Wilson method.
#'
#' @param x the observed numbers of individuals in the sample(s)/population(s)
#'          having the specified characteristic; numeric vector; no default
#' @param n the total number of individuals in the sample(s)/population(s);
#'          numeric vector; no default
#' @param row_label the label to give each row of output (eg area name); character vector, no default
#' @param conf.level the required level of confidence expressed as a number between 0.9 and 1
#'                   or 90 and 100; numeric; default 0.95
#' @param percentage whether the output should be returned as a percentage; logical; default FALSE
#'
#' @return Returns a data frame of numerator, denominator, proportion, lower and upper confidence limits and method
#'
#' @importFrom rlang sym quo_name
#'
#' @examples
#' phe_proportion(65,100, row_label = "dummy")
#' phe_proportion(65,100,99.8,TRUE, row_label = "England - Males")
#' phe_proportion(c(5,65,90,98),c(100,100,100,100), row_label = seq_len(4))
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
phe_proportion <- function(data, x, n, type="combined", conf.level=0.95, percentage=FALSE) {

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
    } else if (any(pull(data, !!x) > pull(data, !!n))) {
        stop("numerators must be less than or equal to denominator for a proportion statistic")
    } else if ((conf.level<0.9)|(conf.level >1 & conf.level <90)|(conf.level > 100)) {
        stop("confidence level must be between 90 and 100 or between 0.9 and 1")
    } else if (!(type %in% c("value", "lower", "upper", "combined", "full"))) {
      stop("type must be one of value, lower, upper, combined or full")
    }

  # scale confidence level
  if (conf.level >= 90) {
    conf.level <- conf.level/100
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
                           confidence = paste(conf.level*100,"%"),
                           method = "Wilson")


  if (type == "lower") {
    phe_proportion <- phe_proportion %>%
      select(-proportion, -uppercl, -confidence, -method)
  } else if (type == "upper") {
    phe_proportion <- phe_proportion %>%
      select(-proportion, -lowercl, -confidence, -method)
  } else if (type == "value") {
    phe_proportion<- phe_proportion %>%
      select(-lowercl, -uppercl, -confidence, -method)
  } else if (type == "combined") {
    phe_proportion <- phe_proportion %>%
      select( -confidence, -method)
  }



return(phe_proportion)
}
