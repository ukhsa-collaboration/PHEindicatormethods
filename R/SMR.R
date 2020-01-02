# -------------------------------------------------------------------------------------------------
#' Calculate Standardised Mortality Ratios using phe_smr
#'
#' Calculates standard mortality ratios (or indirectly standardised ratios) with confidence limits using Byar's [1] or exact [2] CI method.
#'
#' @param data data.frame containing the data to be standardised, pre-grouped if multiple SMRs required; unquoted string; no default
#' @param x_ref the observed number of events in the reference population for each standardisation category
#'              (eg age band); unquoted numeric vector or field name from data depending on value of refpoptype; no default
#' @param n_ref the reference population for each standardisation category (eg age band);
#'              unquoted numeric vector or field name from data depending on value of refpoptype; no default
#' @param refpoptype whether x_ref and n_ref have been specified as vectors or a field name from data;
#'                   quoted string "field" or "vector"; default = "vector"
#' @param refvalue   the standardised reference ratio, numeric, default = 1
#'
#' @inheritParams phe_dsr
#'
#' @import dplyr
#' @export
#'
#' @return When type = "full", returns a tibble of observed events, expected events, standardised mortality ratios,
#'  lower confidence limits, upper confidence limits, confidence level, statistic and method for each grouping set
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
#' df %>%
#'     group_by(indicatorid, year, sex) %>%
#'     phe_smr(obs, pop, refdf$refcount, refdf$refpop, type="standard")
#'
#' ## OR
#'
#' df %>%
#'     group_by(indicatorid, year, sex) %>%
#'     phe_smr(obs, pop, refdf$refcount, refdf$refpop, confidence=99.8, refvalue=100)
#'
#' @section Notes: User MUST ensure that x, n, x_ref and n_ref vectors are all ordered by
#' the same standardisation category values as records will be matched by position. \cr \cr
#' For numerators >= 10 Byar's method [1] is applied using the \code{\link{byars_lower}}
#'  and \code{\link{byars_upper}} functions.  For small
#'  numerators Byar's method is less accurate and so an exact method [2] based
#'  on the Poisson distribution is used.
#'
#' @references
#' [1] Breslow NE, Day NE. Statistical methods in cancer research,
#'  volume II: The design and analysis of cohort studies. Lyon: International
#'  Agency for Research on Cancer, World Health Organisation; 1987. \cr \cr
#' [2] Armitage P, Berry G. Statistical methods in medical research (4th edn).
#'   Oxford: Blackwell; 2002.
#'
#' @family PHEindicatormethods package functions
# -------------------------------------------------------------------------------------------------


phe_smr <- function(data, x, n, x_ref, n_ref, refpoptype = "vector",
                    type = "full", confidence = 0.95, refvalue = 1) {

    # check required arguments present
    if (missing(data)|missing(x)|missing(n)|missing(x_ref)|missing(n_ref)) {
        stop("function phe_smr requires at least 5 arguments: data, x, n, x_ref and n_ref")
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
    if (any(pull(data, {{ x }}) < 0, na.rm = TRUE)) {
        stop("numerators must all be greater than or equal to zero")
    } else if (any(pull(data, {{ n }}) < 0, na.rm = TRUE)) {
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


    # calculate smr and cis and populate metadata fields
    if (length(confidence) == 2) {

        # if two confidence levels requested
        conf1 <- confidence[1]
        conf2 <- confidence[2]

        # calculate smr and CIs
        phe_smr <- data %>%
          mutate(exp_x = na.zero(xrefpop_calc) / nrefpop_calc * na.zero({{ n }})) %>%
          summarise(observed  = sum({{ x }}, na.rm = TRUE),
                    expected  = sum(exp_x)) %>%
          mutate(value     = observed / expected * refvalue,
                 lower95_0cl = if_else(observed < 10, qchisq((1-conf1)/2,2*observed)/2/expected * refvalue,
                                   byars_lower(observed,conf1)/expected * refvalue),
                 upper95_0cl = if_else(observed < 10, qchisq(conf1+(1-conf1)/2,2*observed+2)/2/expected * refvalue,
                                   byars_upper(observed,conf1)/expected * refvalue),
                 lower99_8cl = if_else(observed < 10, qchisq((1-conf2)/2,2*observed)/2/expected * refvalue,
                                   byars_lower(observed,conf2)/expected * refvalue),
                 upper99_8cl = if_else(observed < 10, qchisq(conf2+(1-conf2)/2,2*observed+2)/2/expected * refvalue,
                                   byars_upper(observed,conf2)/expected * refvalue),
                 confidence = paste(conf1 * 100, "%, ", conf2 * 100, "%", sep=""),
                 statistic = paste("smr x ",format(refvalue, scientific=F), sep=""),
                 method  = if_else(observed < 10, "Exact", "Byars"))

        # drop fields not required based on type argument
        if (type == "lower") {
          phe_smr <- phe_smr %>%
            select(-observed, -expected, -value, -upper95_0cl, -upper99_8cl, -confidence, -statistic, -method)
        } else if (type == "upper") {
          phe_smr <- phe_smr %>%
            select(-observed, -expected, -value, -lower95_0cl, -lower99_8cl, -confidence, -statistic, -method)
        } else if (type == "value") {
          phe_smr <- phe_smr %>%
            select(-observed, -expected, -lower95_0cl, -upper95_0cl, -lower99_8cl, -upper99_8cl, -confidence, -statistic, -method)
        } else if (type == "standard") {
          phe_smr <- phe_smr %>%
            select(-confidence, -statistic, -method)
        }

    } else {

      # scale confidence level
      if (confidence[1] >= 90) {
        confidence <- confidence/100
      }

      # calculate SMR and a single CI
        phe_smr <- data %>%
            mutate(exp_x = na.zero(xrefpop_calc) / nrefpop_calc * na.zero({{ n }})) %>%
            summarise(observed = sum({{ x }}, na.rm = TRUE),
                  expected     = sum(exp_x)) %>%
            mutate(value      = observed / expected * refvalue,
                   lowercl    = if_else(observed < 10, qchisq((1-confidence)/2,2*observed)/2/expected * refvalue,
                                    byars_lower(observed,confidence)/expected * refvalue),
                   uppercl    = if_else(observed < 10, qchisq(confidence+(1-confidence)/2,2*observed+2)/2/expected * refvalue,
                                    byars_upper(observed,confidence)/expected * refvalue),
                   confidence = paste(confidence * 100, "%", sep=""),
                   statistic  = paste("smr x ",format(refvalue, scientific=F), sep=""),
                   method     = if_else(observed < 10, "Exact", "Byars"))

        # drop fields not required based on type argument
        if (type == "lower") {
          phe_smr <- phe_smr %>%
            select(-observed, -expected, -value, -uppercl, -confidence, -statistic, -method)
        } else if (type == "upper") {
          phe_smr <- phe_smr %>%
            select(-observed, -expected, -value, -lowercl, -confidence, -statistic, -method)
        } else if (type == "value") {
          phe_smr <- phe_smr %>%
            select(-observed, -expected, -lowercl, -uppercl, -confidence, -statistic, -method)
        } else if (type == "standard") {
          phe_smr <- phe_smr %>%
            select(-confidence, -statistic, -method)
        }

    }

    return(phe_smr)
}
