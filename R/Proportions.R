# -------------------------------------------------------------------------------------------------
#' Calculates a proportion with confidence limits using Wilson method.
#'
#' @param x the observed numbers of individuals in the sample(s)/population(s)
#'          having the specified characteristic; numeric vector; no default
#' @param n the total number of individuals in the sample(s)/population(s);
#'          numeric vector; no default
#' @param conf.level the required level of confidence expressed as a number between 0.9 and 1
#'                   or 90 and 100; numeric; default 0.95
#' @param percentage whether the output should be returned as a percentage; logical; default FALSE
#'
#' @return Returns a data frame of method, numerator, denominator, proportion and confidence limits
#'
#' @examples
#' phe_proportion(65,100)
#' phe_proportion(65,100,99.8,TRUE)
#'
#' @import dplyr
#' @import binom
#' @family phe statistical functions
#' @seealso \code{\link{phe_proportion}} for proportions,
#'          \code{\link{phe_rate}} for rates,
#'          \code{\link{phe_dsr}} for directly standardised rates,
#'          \code{\link{phe_isr}} for indirectly standardised rates and standardised mortality ratios
# -------------------------------------------------------------------------------------------------

# create phe_proportion function to execute binom.confint with method fixed to wilson
phe_proportion <- function(x, n, conf.level=0.95, percentage=FALSE) {

# validate arguments
      if (any(x < 0)) {
              stop("numerators must be greater than or equal to zero")
      } else if (any(n <= 0)) {
              stop("denominators must be greater than zero")
      } else if (any(x > n)) {
              stop("numerators must be less than or equal to denominator for a proportion statistic")
      } else if ((conf.level<0.9)|(conf.level >1 & conf.level <90)|(conf.level > 100)) {
              stop("confidence level must be >= 90 and <= 100 (or >= 0.9 and <= 1)")
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
phe_proportion <- data.frame(binom.confint(x, n, conf.level, methods="wilson")) %>%
        mutate(mean = mean * multiplier) %>%
        mutate(lower = lower * multiplier) %>%
        mutate(upper = upper * multiplier)

# set column names
names(phe_proportion) <- c("method","numerator","denominator","proportion",
                           paste("lower",conf.level*100,"cl",sep=""),
                           paste("upper",conf.level*100,"cl",sep=""))

return(phe_proportion)
}
