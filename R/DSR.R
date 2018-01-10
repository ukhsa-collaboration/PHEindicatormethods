# -------------------------------------------------------------------------------------------------
#' DSR
#'
#' Calculates a directly standardised rate with confidence limits using Dobson method.
#'
#' @param data data.frame containing the data you wish to standardise
#' @param x unquoted field name containing the observed number of events for each standardisation category (eg ageband) within each grouping set (eg area);
#'          numeric vector; no default
#' @param n unquoted field name containing the populations for each standardisation category (eg ageband) within each grouping set (eg area);
#'          numeric vector; no default
#' @param stdpop unquoted field name containing the standard populations for each standardisation category
#'               (eg age band)
#' @param conf.level the required level of confidence expressed as a number between 0.9 and 1
#'                   or 90 and 100; numeric; default 0.95
#' @param multiplier the multiplier used to express the final values (eg 100,000 = rate per 100,000,
#'                   100 = percentage); numeric; default 100,000
#'
#' @return Returns a data frame of numerator, denominator, directly standardised rate,
#'         lower and upper confidence limits and method
#' @importFrom rlang sym quo_name
#' @examples
#' phe_dsr(c(27,45,55,100,125,300,295,270,275,450,455,459,345,300,270,265,100,90,35),
#'         c(84935,80367,72122,79259,99806,87362,81579,71103,70001,
#'           69007,63203,52638,46087,40887,32604,28399,21625,13021,7355), esp2013, conf.level = 0.998)
#' @export
#'
#' @family phe statistical functions
#' @seealso \code{\link{phe_proportion}} for proportions,
#'          \code{\link{phe_rate}} for rates,
#'          \code{\link{phe_mean}} for means,
#'          \code{\link{phe_dsr}} for directly standardised rates,
#'          \code{\link{phe_isr}} for indirectly standardised ratios/rates and standardised mortality ratios
# -------------------------------------------------------------------------------------------------

# define the DSR function
phe_dsr <- function(data, x, n, stdpop, conf.level = 0.95, multiplier = 100000) {

  if (missing(data)) {
    stop("data must contain a data.frame object")
  } else if (missing(x)) {
    stop("x must contain an unquoted field name from data")
  } else if (missing(n)) {
    stop("n must contain an unquoted field name from data")
  }

  x <- enquo(x)
  n <- enquo(n)
  stdpop <- enquo(stdpop)

# validate arguments
  if (any(pull(data, !!x) < 0)) {
      stop("numerators must all be greater than or equal to zero")
  } else if (any(pull(data, !!n) <= 0)) {
      stop("denominators must all be greater than zero")
  } else if ((conf.level<0.9)|(conf.level >1 & conf.level <90)|(conf.level > 100)) {
      stop("confidence level must be between 90 and 100 or between 0.9 and 1")
  } else if (length(pull(data, !!x)) != length(pull(data, !!n))) {
      stop("numerator and denominator vectors must be of equal length")
  } #else if (length(pull(data, !!x)) %% length(stdpop) !=0) {
      #stop("numerator vector length must be a multiple of standard population vector length")
  #}

# scale confidence level
  if (conf.level >= 90) {
    conf.level <- conf.level/100
  }

  lowercl <- paste0("lower",conf.level*100,"cl")
  uppercl <- paste0("upper",conf.level*100,"cl")
# calculate DSR and CIs
  phe_dsr <- data %>%
    mutate(wt_rate = (!!x) * (!!stdpop) / (!!n),
           sq_rate = (!!x) * ((!!stdpop)/(!!n))^2) %>%
    summarise(total_count = sum(!!x),
              total_pop = sum(!!n),
              dsr = sum(wt_rate) / sum((!!stdpop)) * multiplier,
              vardsr = 1/sum(!!stdpop)^2 * sum(sq_rate),
              !!quo_name(lowercl) := dsr + sqrt((vardsr/sum(!!x)))*(byars_lower(sum(!!x),conf.level)-sum(!!x)) * multiplier,
              !!quo_name(uppercl) := dsr + sqrt((vardsr/sum(!!x)))*(byars_upper(sum(!!x),conf.level)-sum(!!x)) * multiplier) %>%
    select(-vardsr) %>%
    mutate(dsr     = if_else(total_count < 10,-1,dsr),
           !!quo_name(lowercl) := if_else(total_count < 10,-1,!!sym(lowercl)),
           !!quo_name(uppercl) := if_else(total_count < 10,-1,!!sym(uppercl)),
           method  = if_else(total_count < 10,"NA","Dobson"))
  # names(phe_dsr[,c("lowercl","uppercl")]) <- c(paste0("lower",conf.level*100,"cl"),
  #                                              paste0("upper",conf.level*100,"cl"))
  #
  #
  # names(phe_dsr) <- c("row_label", "total_count", "total_pop", "dsr",
  #                     paste("lower",conf.level*100,"cl",sep=""),
  #                     paste("upper",conf.level*100,"cl",sep=""),"method")

  return(phe_dsr)

}
