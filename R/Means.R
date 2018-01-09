# -------------------------------------------------------------------------------------------------
#' Calculates a mean with confidence limits using Student's-t distribution method.
#'
#' @param x the observed values in the sample(s)/population(s); numeric vector; no default
#' @param groupref the grouping sets (eg area codes or area names) if calculating multiple means at once,
#'                 character vector, default = No Grouping
#' @param conf.level the required level of confidence expressed as a number between 0.9 and 1
#'                   or 90 and 100; numeric; default 0.95
#'
#' @return Returns a data frame of row labels, sum of values, count of values, mean, standard deviation,
#'         lower and upper confidence limits and method
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
phe_mean <- function(x, groupref = "No grouping", conf.level=0.95) {

  # validate arguments - copied from proportion need editing
  if ((conf.level<0.9)|(conf.level >1 & conf.level <90)|(conf.level > 100)) {
    stop("confidence level must be between 90 and 100 or between 0.9 and 1")
  }

  # scale confidence level
  if (conf.level >= 90) {
    conf.level <- conf.level/100
  }

  p <- (1 - conf.level) / 2

  # calculate proportion and CIs
  phe_mean <- data.frame(x, groupref) %>%
       group_by(groupref) %>%
       summarise(total   = sum(x),
                 numrecs = length(x),
                 stdev   = sd(x)) %>%
       mutate(mean = total / numrecs,
              lowercl = mean - abs(qt(p, numrecs - 1)) * stdev / sqrt(numrecs),
              uppercl = mean + abs(qt(p, numrecs - 1)) * stdev / sqrt(numrecs),
              method  = "t-distribution") %>%
    select(1,2,3,5,4,6,7,8)

  # set column names
  names(phe_mean) <- c("row_label","value_sum","value_count","mean","stdev",
                             paste("lower",conf.level*100,"cl",sep=""),
                             paste("upper",conf.level*100,"cl",sep=""),"method")

  return(phe_mean)
}
