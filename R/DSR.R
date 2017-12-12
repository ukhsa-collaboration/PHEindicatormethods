# -------------------------------------------------------------------------------------------------
#' Calculates a directly standardised rate with confidence limits using Dobson method.
#'
#' @param x the observed number of events for each standardisation category (eg ageband) within each group (eg area);
#'          numeric vector; no default
#' @param n the populations for each standardisation category (eg ageband) within each group (eg area);
#'          numeric vector; no default
#' @param stdpop the standard populations for each standardisation category
#'               (eg age band); numeric vector; default is the European Standard Population 2013
#'              divided into 19 five-year agebands: 0-4, 5-9, 10-14, .....90+
#' @param groupref the
#' @param conf.level the required level of confidence expressed as a number between 0.9 and 1
#'                   or 90 and 100; numeric; default 0.95
#' @param multiplier the multiplier used to express the final values (eg 100,000 = rate per 100,000,
#'                   100 = percentage); numeric; default 100,000
#'
#' @export
#'
#' @return Returns a data frame of method, numerator, denominator, directly standardised rate
#'         and confidence interval limits
#'
#' @examples
#' phe_dsr(c(27,45,55,100,125,300,295,270,275,450,455,459,345,300,270,265,100,90,35),
#'         c(84935,80367,72122,79259,99806,87362,81579,71103,70001,
#'           69007,63203,52638,46087,40887,32604,28399,21625,13021,7355),conf.level = 0.998)
# -------------------------------------------------------------------------------------------------

# define the DSR function
phe_dsr <- function(x,n,stdpop = esp2013, groupref = 1, conf.level = 0.95, multiplier = 100000) {

# validate arguments
  if (any(x < 0)) {
    stop("numerators must all be greater than or equal to zero")
  } else if (any(n <= 0)) {
    stop("denominators must all be greater than zero")
  } else if ((conf.level<0.9)|(conf.level >1 & conf.level <90)|(conf.level > 100)) {
    stop("confidence level must be >= 90 and <= 100 (or >= 0.9 and <= 1)")
  } else if (length(x) != length(n)) {
    stop("numerator and denominator vectors must be of equal length")
  } else if (length(x) %% length(stdpop) !=0) {
    stop("numerator vector length must be a multiple of standard population vector length")
  } else if (sum(x) < 10) {
    stop("DSR calculation is not valid for total counts < 10")
  }

# scale confidence level
  if (conf.level >= 90) {
    conf.level <- conf.level/100
  }


# calculate DSR and CIs
  phe_dsr <- data.frame(x, n, stdpop, groupref) %>%
    group_by(groupref) %>%
    mutate(wt_rate = x * stdpop / n,
           sq_rate = x * (stdpop/n)^2) %>%
    summarise(total_count = sum(x),
              total_pop = sum(n),
              dsr = sum(wt_rate) / sum(stdpop) * multiplier,
              vardsr = 1/sum(stdpop)^2 * sum(sq_rate),
              lowercl = dsr + sqrt((vardsr/sum(x)))*(byars_lower(sum(x),conf.level)-sum(x)) * multiplier,
              uppercl = dsr + sqrt((vardsr/sum(x)))*(byars_upper(sum(x),conf.level)-sum(x)) * multiplier) %>%
    mutate(method = "Dobson") %>%
    select(method, groupref, total_count, total_pop, dsr, lowercl, uppercl)


  # remember to don't output dsr or cis for sum(x) < 10

  names(phe_dsr) <- c("method", "group", "total_count", "total_pop", "dsr",
                      paste("lower",conf.level*100,"cl",sep=""),
                      paste("upper",conf.level*100,"cl",sep=""))

  return(phe_dsr)

}
