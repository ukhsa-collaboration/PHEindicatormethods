# -------------------------------------------------------------------------------------------------
#' Calculates an indirectly standardised ratio or rate with confidence limits using Byars or Exact CI method.
#'
#' @param x the observed number of events for each standardisation category (eg ageband) within each grouping set (eg area);
#'          numeric vector; no default
#' @param n the populations for each standardisation category (eg ageband) within each grouping set (eg area);
#'          numeric vector; no default
#' @param x_ref the observed number of events in the reference population for each standardisation category
#'               (eg age band) within each grouping set (eg area); numeric vector; no default
#' @param n_ref the reference population for each standardisation category
#'               (eg age band) within each grouping set (eg area); numeric vector; no default
#' @param groupref the grouping sets (eg area codes or area names) if calculating multiple ISRs at once;
#'                 character vector; default = No Grouping
#' @param conf.level the required level of confidence expressed as a number between 0.9 and 1
#'                   or 90 and 100; numeric; default 0.95
#' @param ratio whether the output should be expressed as a ratio (TRUE) or a rate (FALSE); logical; default = TRUE
#' @param ref_0 when ratio = TRUE - whether the ratios should be compared to a reference ratio of 0 or 100; logical, default=TRUE
#' @param multiplier when ratio = FALSE - the multiplier used to express the final ISRs (eg 100,000 = rate per 100,000,
#'                   100 = percentage); numeric; default 100,000
#'
#' @return When ratio = TRUE   - returns a data frame of row label, observed count, expected count,
#'                             indirectly standardised rate, lower and upper confidence interval limits and method
#'         When ratio = FALSE  - returns a data frame of row label, observed count, expected count,
#'                             indirectly standardised rate, lower and upper confidence interval limits, reference rate, and method
#'
#' @examples
#' phe_isr(c(27,45,55,100,125,300,295,270,275,450,455,459,345,300,270,265,100,90,35),
#'         c(84935,80367,72122,79259,99806,87362,81579,71103,70001,
#'           69007,63203,52638,46087,40887,32604,28399,21625,13021,7355),
#'         c(10303,2824,3225,3615,3641,3490,3789,3213,3031,2771,3089,3490,3595,4745,5514,7125,5694,6210,5757),
#'         c(50520,57173,60213,54659,44345,50128,62163,67423,62899,55463,60479,49974,
#'           44140,40888,37239,30819,18136,15325,13918))
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


phe_isr <- function(x,n,x_ref, n_ref, groupref = "No Grouping", conf.level = 0.95,
                    ratio = TRUE, ref_0 = TRUE, multiplier = 100000) {

  # validate arguments
  if (any(x < 0)) {
      stop("numerators must all be greater than or equal to zero")
  } else if (any(n <= 0)) {
      stop("denominators must all be greater than zero")
  } else if (any(x_ref < 0)) {
      stop("reference numerators must all be greater than or equal to zero")
  } else if (any(n_ref <= 0)) {
      stop("reference denominators must all be greater than zero")
  } else if ((conf.level<0.9) | (conf.level >1 & conf.level <90)|(conf.level > 100)) {
      stop("confidence level must be between 90 and 100 or between 0.9 and 1")
  } else if (length(x) != length(n)) {
      stop("numerator and denominator vectors must be of equal length")
  } else if (length(x_ref) != length(n_ref)) {
      stop("reference numerator and reference denominator vectors must be of equal length")
  } else if (length(x) %% length(x_ref) !=0) {
      stop("numerator vector length must be a multiple of the reference numerator vector length")
  }

  # scale confidence level
  if (conf.level >= 90) {
    conf.level <- conf.level/100
  }

  # loop for ratios
  if (ratio==TRUE) {

    #set multiplier
    multiplier <- 1
    if (ref_0 == FALSE) {
      multiplier <- 100
    }

    # calculate ISR and CIS to output as ratio
    phe_isr <- data.frame(x,n,x_ref,n_ref,groupref) %>%
    mutate(exp_x = x_ref/n_ref * n) %>%
    group_by(groupref) %>%
    summarise(obs  = sum(x),
              exp  = sum(exp_x),
              isr  = obs / exp * multiplier) %>%
    mutate(lowercl = if_else(obs<10, qchisq((1-conf.level)/2,2*obs)/2/exp*multiplier,
                             byars_lower(obs,conf.level)/exp*multiplier),
           uppercl = if_else(obs<10, qchisq(conf.level+(1-conf.level)/2,2*obs+2)/2/exp*multiplier,
                             byars_upper(obs,conf.level)/exp*multiplier),
           method  = if_else(obs<10,"Exact","Byars"))

    names(phe_isr) <- c("group", "observed", "expected", "isr",
                      paste("lower",conf.level*100,"cl",sep=""),
                      paste("upper",conf.level*100,"cl",sep=""),"method")
  }

  # calculate ISR and CIs to output as rate
  if (ratio == FALSE) {

    phe_isr <- data.frame(x,n,x_ref,n_ref,groupref) %>%
      mutate(exp_x = x_ref/n_ref * n) %>%
      group_by(groupref) %>%
      summarise(obs  = sum(x),
                exp  = sum(exp_x),
                ref_rate = sum(x_ref) / sum(n_ref) * multiplier) %>%
      mutate(isr     = obs / exp * ref_rate,
             lowercl = if_else(obs<10, qchisq((1-conf.level)/2,2*obs)/2/exp * ref_rate,
                               byars_lower(obs,conf.level)/exp * ref_rate),
             uppercl = if_else(obs<10, qchisq(conf.level+(1-conf.level)/2,2*obs+2)/2/exp * ref_rate,
                               byars_upper(obs,conf.level)/exp * ref_rate),
             method  = if_else(obs<10,"Exact","Byars")) %>%
             select(1:3,5:7,4,8)

   names(phe_isr) <- c("row_label", "observed", "expected", "isr",
                        paste("lower",conf.level*100,"cl",sep=""),
                        paste("upper",conf.level*100,"cl",sep=""), "reference rate", "method")

  }

  return(phe_isr)
}

