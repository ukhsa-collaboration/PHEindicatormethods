# -------------------------------------------------------------------------------------------------
#' phe_isr
#'
#' Calculates an indirectly standardised rate with confidence limits using Byars or Exact CI method.
#'
#' @param data data.frame containing the data to be standarised, pre-grouped if multiple ISRs required; unquoted string; no default
#' @param x_ref the observed number of events in the reference population for each standardisation category
#'              (eg age band) within each grouping set (eg area). If reference populations are held within data,
#'              the column must be referenced as a vector and not repeated eg df$x_ref[1:19]; numeric vector; no default
#' @param n_ref the reference population for each standardisation category
#'              (eg age band) within each grouping set (eg area).  If reference populations are held within data,
#'              the column must be referenced as a vector and not repeated eg df$n_ref[1:19]; numeric vector; no default
#'
#' @inheritParams phe_dsr
#'
#' @return When type = "full", returns a data frame of observed, expected, ref_rate, value, lowercl, uppercl, confidence, statistic and method
#'         for each grouping set
#'
#' @examples
#' NEED TO ADD EXAMPLES
#'
#' @export
#'
#' @family PHEstatmethods package functions
# -------------------------------------------------------------------------------------------------


phe_isr <- function(data, x, n, x_ref, n_ref, type = "standard", confidence = 0.95, multiplier = 100000) {

  # check required arguments present
  if (missing(data)|missing(x)|missing(n)|missing(x_ref)|missing(n_ref)) {
    stop("function phe_isr requires at least 5 arguments: data, x, n, x_ref and n_ref")
  }

  # apply quotes
  x <- enquo(x)
  n <- enquo(n)
  enquox_ref <- enquo(x_ref)
  enquon_ref <- enquo(n_ref)

  # validate arguments
  if (any(pull(data, !!x) < 0)) {
      stop("numerators must all be greater than or equal to zero")
  } else if (any(pull(data, !!n) <= 0)) {
      stop("denominators must all be greater than zero")
  } else if ((confidence<0.9)|(confidence >1 & confidence <90)|(confidence > 100)) {
      stop("confidence level must be between 90 and 100 or between 0.9 and 1")
  } else if (!(type %in% c("value", "lower", "upper", "standard", "full"))) {
      stop("type must be one of value, lower, upper, standard or full")
  } else if (n_distinct(select(ungroup(summarise(data,n=n())),n)) != 1) {
      stop("data must contain the same number of rows for each group")
  }  else if (pull(slice(select(ungroup(summarise(data,n=n())),n),1)) != length(x_ref)) {
      stop("x_ref length must equal number of rows in each group within data")
  }  else if (pull(slice(select(ungroup(summarise(data,n=n())),n),1)) != length(n_ref)) {
    stop("n_ref length must equal number of rows in each group within data")
  }


  # scale confidence level
  if (confidence >= 90) {
    confidence <- confidence/100
  }

  phe_isr <- data %>%
    mutate(exp_x = (!!x_ref)/(!!n_ref) * (!!n)) %>%
    summarise(observed  = sum((!!x)),
              expected  = sum(exp_x),
              ref_rate = sum((!!x_ref)) / sum((!!n_ref)) * multiplier) %>%
    mutate(value     = observed / expected * ref_rate,
           lowercl = if_else(observed<10, qchisq((1-confidence)/2,2*observed)/2/expected * ref_rate,
                             byars_lower(observed,confidence)/expected * ref_rate),
           uppercl = if_else(observed<10, qchisq(confidence+(1-confidence)/2,2*observed+2)/2/expected * ref_rate,
                             byars_upper(observed,confidence)/expected * ref_rate),
           confidence = paste(confidence*100,"%"),
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

