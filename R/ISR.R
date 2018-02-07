# -------------------------------------------------------------------------------------------------
#' phe_isr
#'
#' Calculates an indirectly standardised rate with confidence limits using Byars or Exact CI method.
#'
#' @param data data.frame containing the data to be standarised, pre-grouped if multiple ISRs required; unquoted string; no default
#' @param x_ref the observed number of events in the reference population for each standardisation category
#'              (eg age band); unquoted numeric vector or field name from data depending on value of refpoptype argument; no default
#' @param n_ref the reference population for each standardisation category (eg age band);
#'              unquoted numeric vector or field name from data depending on value of refpoptype argument; no default
#' @param refpoptype whether x_ref and n_ref have been specified as vectors or a field name from data argument;
#'                   quoted string "field" or "vector"; default = vector
#'
#' @inheritParams phe_dsr
#'
#' @return When type = "full", returns a data frame of observed, expected, value, lowercl, uppercl, confidence, statistic and method
#'         for each grouping set
#'
#' @examples
#' library(dplyr)
#' df <- data.frame(indicatorid = rep(c(1234, 5678, 91011, 121314), each = 19 * 2 * 5),
#'                  year = rep(2006:2010, each = 19 * 2),
#'                  sex = rep(rep(c("Male", "Female"), each = 19), 5),
#'                  ageband = rep(c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90), times = 10),
#'                  obs = sample(200, 19 * 2 * 5 * 4, replace = TRUE),
#'                  pop = sample(10000:20000, 19 * 2 * 5 * 4, replace = TRUE))
#'
#' refdf <- data.frame(refcount = sample(200, 19, replace = TRUE),
#'                     refpop = sample(10000:20000, 19, replace = TRUE))
#'
#' df %>%
#'     group_by(indicatorid, year, sex) %>%
#'     phe_isr(df, obs, pop, refdf$refcount, refdf$refpop)
#'
#' ## OR
#'
#' df %>%
#'     group_by(indicatorid, year, sex) %>%
#'     phe_isr(df, obs, pop, refdf$refcount, refdf$refpop, type="full", confidence=99.8)
#'
#' @export
#'
#' @family PHEstatmethods package functions
# -------------------------------------------------------------------------------------------------


phe_isr <- function(data, x, n, x_ref, n_ref, refpoptype = "vector", type = "standard", confidence = 0.95, multiplier = 100000) {

  # check required arguments present
  if (missing(data)|missing(x)|missing(n)|missing(x_ref)|missing(n_ref)) {
    stop("function phe_isr requires at least 5 arguments: data, x, n, x_ref and n_ref")
  }

  # check same number of rows per group
  if (n_distinct(select(ungroup(summarise(data,n=n())),n)) != 1) {
    stop("data must contain the same number of rows for each group")
  }

  # check ref pops are valid and append to data
  if (!(refpoptype %in% c("vector","field"))) {
    stop("valid values for refpoptype are vector and field")
  } else if (refpoptype == "vector") {
    if (pull(slice(select(ungroup(summarise(data,n=n())),n),1)) != length(x_ref)) {
      stop("x_ref length must equal number of rows in each group within data")
    } else if (pull(slice(select(ungroup(summarise(data,n=n())),n),1)) != length(n_ref)) {
        stop("n_ref length must equal number of rows in each group within data")
      }
    data <- mutate(data,xrefpop_calc = x_ref,
                        nrefpop_calc = n_ref)
  } else if (refpoptype == "field") {
    enquoxref <- enquo(x_ref)
    enquonref <- enquo(n_ref)
    if (deparse(substitute(x_ref)) %in% colnames(data)) {
      if(deparse(substitute(n_ref)) %in% colnames(data)) {
        data <- mutate(data,xrefpop_calc = !!enquoxref,
                            nrefpop_calc = !!enquonref)
      } else stop("n_ref is not a field name from data")
    } else stop("x_ref is not a field name from data")
  }

  # apply quotes
  x <- enquo(x)
  n <- enquo(n)

  # validate arguments
  if (any(pull(data, !!x) < 0)) {
      stop("numerators must all be greater than or equal to zero")
  } else if (any(pull(data, !!n) <= 0)) {
      stop("denominators must all be greater than zero")
  } else if ((confidence<0.9)|(confidence >1 & confidence <90)|(confidence > 100)) {
      stop("confidence level must be between 90 and 100 or between 0.9 and 1")
  } else if (!(type %in% c("value", "lower", "upper", "standard", "full"))) {
      stop("type must be one of value, lower, upper, standard or full")
  }


  # scale confidence level
  if (confidence >= 90) {
    confidence <- confidence/100
  }

  phe_isr <- data %>%
    mutate(exp_x = xrefpop_calc/nrefpop_calc * (!!n)) %>%
    summarise(observed  = sum((!!x)),
              expected  = sum(exp_x),
              ref_rate = sum(xrefpop_calc) / sum(nrefpop_calc) * multiplier) %>%
    mutate(value     = observed / expected * ref_rate,
           lowercl = if_else(observed<10, qchisq((1-confidence)/2,2*observed)/2/expected * ref_rate,
                             byars_lower(observed,confidence)/expected * ref_rate),
           uppercl = if_else(observed<10, qchisq(confidence+(1-confidence)/2,2*observed+2)/2/expected * ref_rate,
                             byars_upper(observed,confidence)/expected * ref_rate),
           confidence = paste(confidence*100,"%", sep=""),
           statistic = paste("isr per",format(multiplier,scientific=F)),
           method  = if_else(observed<10,"Exact","Byars"))

  if (type == "lower") {
    phe_isr <- phe_isr %>%
      select(-observed, -expected, -ref_rate, -value, -uppercl, -confidence, -statistic, -method)
  } else if (type == "upper") {
    phe_isr <- phe_isr %>%
      select(-observed, -expected, -ref_rate, -value, -lowercl, -confidence, -statistic, -method)
  } else if (type == "value") {
    phe_isr <- phe_isr %>%
      select(-observed, -expected, -ref_rate, -lowercl, -uppercl, -confidence, -statistic, -method)
  } else if (type == "standard") {
    phe_isr <- phe_isr %>%
      select(-observed, -expected, -ref_rate, -confidence, -statistic, -method)
  }

  return(phe_isr)
}

