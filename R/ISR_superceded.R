# -------------------------------------------------------------------------------------------------
#' Calculate Indirectly Standardised Rates using calculate_ISRate
#'
#' @description `r lifecycle::badge("superseded")`
#'
#' To prevent ambiguity over the ISR acronymn, the `phe_isr` function has been
#' replaced with a `calculate_ISRate` function. Both functions take the same
#' parameters and provide identical functionality.  The `phe_isr` function will
#' remain available in the `PHEindicatormethods` package until at least December
#' 2022 to allow users time to transfer existing code to use the new
#' `calculate_ISRate` function. Please refer to the `calculate_ISRate`
#' documentation for full support on the use of these functions.
#'
#' @inheritParams calculate_ISRate
#'
#' @import dplyr lifecycle
#' @export
#'
#' @examples
#' library(dplyr)
#' df <- data.frame(indicatorid = rep(c(1234, 5678, 91011, 121314), each = 19 * 2 * 5),
#'                  year = rep(2006:2010, each = 19 * 2),
#'                  sex = rep(rep(c("Male", "Female"), each = 19), 5),
#'                  ageband = rep(c(0,5,10,15,20,25,30,35,40,45,
#'                                  50,55,60,65,70,75,80,85,90), times = 10),
#'                  obs = sample(200, 19 * 2 * 5 * 4, replace = TRUE),
#'                  pop = sample(10000:20000, 19 * 2 * 5 * 4, replace = TRUE))
#'
#' refdf <- data.frame(refcount = sample(200, 19, replace = TRUE),
#'                     refpop = sample(10000:20000, 19, replace = TRUE))
#'
#' ## calculate multiple ISRs in single execution
#' df %>%
#'     group_by(indicatorid, year, sex) %>%
#'     phe_isr(obs, pop, refdf$refcount, refdf$refpop)
#'
#' ## execute without outputting metadata fields
#' df %>%
#'     group_by(indicatorid, year, sex) %>%
#'     phe_isr(obs, pop, refdf$refcount, refdf$refpop, type="standard", confidence=99.8)
#'
#' ## calculate 95% and 99.8% CIs in single execution
#' df %>%
#'     group_by(indicatorid, year, sex) %>%
#'     phe_isr(obs, pop, refdf$refcount, refdf$refpop, confidence = c(0.95, 0.998))
#'
# -------------------------------------------------------------------------------------------------

phe_isr <- function(data, x, n, x_ref, n_ref, refpoptype = "vector",
                             type = "full", confidence = 0.95, multiplier = 100000) {

  # check required arguments present
  if (missing(data)|missing(x)|missing(n)|missing(x_ref)|missing(n_ref)) {
    stop("function phe_isr requires at least 5 arguments: data, x, n, x_ref and n_ref")
  }


  # check same number of rows per group
  if (n_distinct(select(ungroup(count(data)),n)) != 1) {
    stop("data must contain the same number of rows for each group")
  }


  # check ref pops are valid and append to data
  if (!(refpoptype %in% c("vector","field"))) {
    stop("valid values for refpoptype are vector and field")

  } else if (refpoptype == "vector") {
    if (pull(slice(select(ungroup(count(data)),n),1)) != length(x_ref)) {
      stop("x_ref length must equal number of rows in each group within data")
    } else if (pull(slice(select(ungroup(count(data)),n),1)) != length(n_ref)) {
      stop("n_ref length must equal number of rows in each group within data")
    }
    data <- mutate(data,xrefpop_calc = x_ref,
                   nrefpop_calc = n_ref)

  } else if (refpoptype == "field") {
    if (deparse(substitute(x_ref)) %in% colnames(data)) {
      if(deparse(substitute(n_ref)) %in% colnames(data)) {
        data <- mutate(data,xrefpop_calc = {{ x_ref }},
                       nrefpop_calc = {{ n_ref }})
      } else stop("n_ref is not a field name from data")
    } else stop("x_ref is not a field name from data")
  }


  # validate arguments
  if (any(pull(data, {{ x }}) < 0, na.rm=TRUE)) {
    stop("numerators must all be greater than or equal to zero")
  } else if (any(pull(data, {{ n }}) < 0, na.rm=TRUE)) {
    stop("denominators must all be greater than or equal to zero")
  } else if (!(type %in% c("value", "lower", "upper", "standard", "full"))) {
    stop("type must be one of value, lower, upper, standard or full")
  } else if (length(confidence) >2) {
    stop("a maximum of two confidence levels can be provided")
  } else if (length(confidence) == 2) {
    if (!(confidence[1] == 0.95 & confidence[2] == 0.998)) {
      stop("two confidence levels can only be produced if they are specified as 0.95 and 0.998")
    }
  } else if ((confidence < 0.9)|(confidence > 1 & confidence < 90)|(confidence > 100)) {
    stop("confidence level must be between 90 and 100 or between 0.9 and 1")
  }


  # calculate the isr and populate metadata fields
  if (length(confidence) == 2) {

    # if two confidence levels requested
    conf1 <- confidence[1]
    conf2 <- confidence[2]

    # calculate isr and CIs
    phe_isr <- data %>%
      mutate(exp_x = na.zero(xrefpop_calc)/nrefpop_calc * na.zero({{ n }})) %>%
      summarise(observed  = sum({{ x }}, na.rm=TRUE),
                expected  = sum(exp_x),
                ref_rate = sum(xrefpop_calc, na.rm=TRUE) / sum(nrefpop_calc) * multiplier,
                .groups = "keep") %>%
      mutate(value     = observed / expected * ref_rate,
             lower95_0cl = if_else(observed<10, qchisq((1-conf1)/2,2*observed)/2/expected * ref_rate,
                                   byars_lower(observed,conf1)/expected * ref_rate),
             upper95_0cl = if_else(observed<10, qchisq(conf1+(1-conf1)/2,2*observed+2)/2/expected * ref_rate,
                                   byars_upper(observed,conf1)/expected * ref_rate),
             lower99_8cl = if_else(observed<10, qchisq((1-conf2)/2,2*observed)/2/expected * ref_rate,
                                   byars_lower(observed,conf2)/expected * ref_rate),
             upper99_8cl = if_else(observed<10, qchisq(conf2+(1-conf2)/2,2*observed+2)/2/expected * ref_rate,
                                   byars_upper(observed,conf2)/expected * ref_rate),
             confidence = "95%, 99.8%",
             statistic = paste("indirectly standardised rate per",format(multiplier,scientific=F)),
             method  = if_else(observed<10,"Exact","Byars"))

    # drop fields not required based on value of type argument
    if (type == "lower") {
      phe_isr <- phe_isr %>%
        select(-observed, -expected, -ref_rate, -value, -upper95_0cl, -upper99_8cl, -confidence, -statistic, -method)
    } else if (type == "upper") {
      phe_isr <- phe_isr %>%
        select(-observed, -expected, -ref_rate, -value, -lower95_0cl, -lower99_8cl, -confidence, -statistic, -method)
    } else if (type == "value") {
      phe_isr <- phe_isr %>%
        select(-observed, -expected, -ref_rate, -lower95_0cl, -lower99_8cl, -upper95_0cl, -upper99_8cl, -confidence, -statistic, -method)
    } else if (type == "standard") {
      phe_isr <- phe_isr %>%
        select(-confidence, -statistic, -method)
    }

  } else {

    # scale confidence level
    if (confidence >= 90) {
      confidence <- confidence/100
    }


    # calculate isr with a single CI
    phe_isr <- data %>%
      mutate(exp_x = na.zero(xrefpop_calc)/nrefpop_calc * na.zero({{ n }})) %>%
      summarise(observed  = sum({{ x }}, na.rm=TRUE),
                expected  = sum(exp_x),
                ref_rate = sum(xrefpop_calc, na.rm=TRUE) / sum(nrefpop_calc) * multiplier,
                .groups = "keep") %>%
      mutate(value     = observed / expected * ref_rate,
             lowercl = if_else(observed<10, qchisq((1-confidence)/2,2*observed)/2/expected * ref_rate,
                               byars_lower(observed,confidence)/expected * ref_rate),
             uppercl = if_else(observed<10, qchisq(confidence+(1-confidence)/2,2*observed+2)/2/expected * ref_rate,
                               byars_upper(observed,confidence)/expected * ref_rate),
             confidence = paste(confidence*100,"%", sep=""),
             statistic = paste("indirectly standardised rate per",format(multiplier,scientific=F)),
             method  = if_else(observed<10,"Exact","Byars"))

    # drop fields not required based on value of type argument
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
        select(-confidence, -statistic, -method)
    }

  }

  return(phe_isr)
}


