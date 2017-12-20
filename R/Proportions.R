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
#' @examples
#' phe_proportion(65,100)
#' phe_proportion(65,100,99.8,TRUE)
#' phe_proportion(c(5,65,90,98),c(100,100,100,100))
#'
#' @import dplyr
#' @import binom
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
phe_proportion <- function(x, n, row_label, conf.level=0.95, percentage=FALSE) {

  # validate arguments
  if (any(x < 0)) {
        stop("numerators must be greater than or equal to zero")
    } else if (any(n <= 0)) {
        stop("denominators must be greater than zero")
    } else if (any(x > n)) {
        stop("numerators must be less than or equal to denominator for a proportion statistic")
    } else if ((conf.level<0.9)|(conf.level >1 & conf.level <90)|(conf.level > 100)) {
        stop("confidence level must be between 90 and 100 or between 0.9 and 1")
    } else if (length(x) != length(n)) {
        stop("numerator and denominator vectors must be of equal length")
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
  phe_proportion <- data.frame(row_label, binom.confint(x, n, conf.level, methods="wilson")) %>%
                    mutate(mean = mean * multiplier,
                           lower = lower * multiplier,
                           upper = upper * multiplier) %>%
                    select(1,3:7,2)

  # set column names
  names(phe_proportion) <- c("row_label","numerator","denominator","proportion",
                           paste("lower",conf.level*100,"cl",sep=""),
                           paste("upper",conf.level*100,"cl",sep=""),"method")

return(phe_proportion)
}
