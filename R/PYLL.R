# -------------------------------------------------------------------------------------------------
#' Calculate potential years of life lost using phe_pyll
#'
#' Calculates potential years of life lost rates with confidence limits using Dobson & Byar's (PYLL variation) method.
#'
#' @param data data.frame containing the data to be standardised, pre-grouped if
#'   multiple PYLL required; unquoted string; no default
#' @param x field name from data containing the observed number of events for
#'   each standardisation category (eg ageband) within each grouping set (eg
#'   area); unquoted string; no default
#' @param n field name from data containing the populations for each
#'   standardisation category (eg ageband) within each grouping set (eg area);
#'   unquoted string; no default
#'  @param leadj the weight or average age-specific period life expectancy for
#'  each standardisation category (eg ageband) within each grouping set (eg area)
#'  unquoted string; no default
#' @param stdpop the standard populations for each standardisation category (eg
#'   age band); unquoted string referencing a numeric vector or field name from
#'   data depending on value of stdpoptype; default = esp2013
#' @param stdpoptype whether the stdpop has been specified as a vector or a
#'   field name from data; quoted string "field" or "vector"; default = "vector"
#' @param type defines the data and metadata columns to be included in output;
#'   can be "value", "lower", "upper", "standard" (for all data) or "full" (for
#'   all data and metadata); quoted string; default = "full"
#' @param confidence the required level of confidence expressed as a number
#'   between 0.9 and 1 or a number between 90 and 100 or can be a vector of 0.95
#'   and 0.998, for example, to output both 95 percent and 99.8 percent percent CIs; numeric;
#'   default 0.95
#' @param multiplier the multiplier used to express the final values (eg 100,000
#'   = rate per 100,000); numeric; default 100,000
#'
#' @return When type = "full", returns a tibble of total counts, total
#'   populations, directly standardised rates, lower confidence limits, upper
#'   confidence limits, confidence level, statistic and method for each grouping
#'   set
#'
#' @importFrom rlang sym quo_name
#' @import dplyr
#' @export
#'
#' @examples
#' library(dplyr)
#' df <- data.frame(indicatorid = rep(c(1234, 5678, 91011, 121314),
#'                  each = 19 * 2 * 5),
#'                  year = rep(2006:2010, each = 19 * 2),
#'                  sex = rep(rep(c("Male", "Female"), each = 19), 5),
#'                  ageband = rep(c(0,5,10,15,20,25,30,35,40,45,
#'                                  50,55,60,65,70,75,80,85,90), times = 10),
#'                  obs = sample(200, 19 * 2 * 5 * 4, replace = TRUE),
#'                  pop = sample(10000:20000, 19 * 2 * 5 * 4, replace = TRUE))
#'
#' ## default execution
#' df %>%
#'     group_by(indicatorid, year, sex) %>%
#'     phe_pyll(obs, pop, leadj)
#'
#' ## calculate both 95% and 99.8% CIs in single execution
#' df %>%
#'     group_by(indicatorid, year, sex) %>%
#'     phe_pyll(obs, pop, leadj confidence = c(0.95, 0.998))
#'
#' ## calculate DSRs for multiple grouping sets in single execution
#'
#' df %>%
#'     group_by(indicatorid, year, sex) %>%
#'     phe_pyll(obs, pop, leadj type = "standard")
#'
#' @section Notes: User MUST ensure that x, n leadj and stdpop vectors are all ordered
#'   by the same standardisation category values as records will be matched by
#'   position. \cr \cr For total counts >= 10 Byar's method (1) is applied using
#'   the \code{\link{byars_lower}} and \code{\link{byars_upper}} functions.
#'   When the total count is < 10 pyll are not reliable and will therefore not
#'   be calculated.- because numerator is years not deaths disclosure not applied REMOVE
#'
#' @references
#' (1) Breslow NE, Day NE. Statistical methods in cancer research,
#'  volume II: The design and analysis of cohort studies. Lyon: International
#'  Agency for Research on Cancer, World Health Organisation; 1987. \cr \cr
#' (2) Dobson A et al. Confidence intervals for weighted sums of Poisson parameters. Stat Med 1991;10:457-62.
#'
#'
#' @family PHEindicatormethods package functions
# -------------------------------------------------------------------------------------------------

# define the pyll function using Dobson method
phe_pyll <- function(data, x, n, leadj, stdpop = esp2013, stdpoptype = "vector",
                    type = "full", confidence = 0.95, multiplier = 100000) {

  # check required arguments present
  if (missing(data)|missing(x)|missing(n)) {
    stop("function phe_pyll requires at least 4 arguments: data, x, n, leadj")
  }

  # check same number of rows per group
  if (n_distinct(select(ungroup(count(data)),n)) != 1) {
    stop("data must contain the same number of rows for each group")
  }

  # check stdpop is valid and append to data
  if (!(stdpoptype %in% c("vector","field"))) {
    stop("valid values for stdpoptype are vector and field")

  } else if (stdpoptype == "vector") {
    if (pull(slice(select(ungroup(count(data)),n),1)) != length(stdpop)) {
      stop("stdpop length must equal number of rows in each group within data")
    }
    data <- mutate(data,stdpop_calc = stdpop)

  } else if (stdpoptype == "field") {
    if (deparse(substitute(stdpop)) %in% colnames(data)) {
      data <- mutate(data,stdpop_calc = {{ stdpop }} )
    } else stop("stdpop is not a field name from data")
  }


  # validate arguments
  if (any(pull(data, {{ x }}) < 0, na.rm=TRUE)) {
    stop("numerators must all be greater than or equal to zero")
  } else if (any(pull(data, {{ n }}) <= 0)) {
    stop("denominators must all be greater than zero")
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


  # calculate PYLL and CIs
  if (length(confidence) == 2) {

    # if two confidence levels requested
    conf1 <- confidence[1]
    conf2 <- confidence[2]

    # calculate PYLL and CIs
    phe_pyll <- data %>%
      mutate(wt_rate = na.zero({{ x }}) *  stdpop_calc / ({{ n }}),
             sq_rate = na.zero({{ x }}) * (stdpop_calc / ({{ n }}))^2, na.rm=TRUE) %>%
      summarise(total_count = sum({{ x }},na.rm=TRUE),
                total_pop = sum({{ n }}),
                value = sum(wt_rate) / sum(stdpop_calc) * multiplier,
                err_frac = sum(sum(stdpop_calc)^2 * sum(x)*sum(leadj)^2),
                pyll_lower95_0cl = value + sqrt((err_frac/sum({{ x }}, na.rm=TRUE)))*
                  (byars_lower(sum({{ x }}, na.rm=TRUE), conf1) - sum({{ x }}, na.rm=TRUE)) * multiplier,
                pyll_upper95_0cl = value + sqrt((err_frac/sum({{ x }}, na.rm=TRUE)))*
                  (byars_upper(sum({{ x }}, na.rm=TRUE), conf1) - sum({{ x }}, na.rm=TRUE)) * multiplier,
                pyll_lower99_8cl = value + sqrt((err_frac/sum({{ x }}, na.rm=TRUE)))*
                  (byars_lower(sum({{ x }}, na.rm=TRUE), conf2) - sum({{ x }}, na.rm=TRUE)) * multiplier,
                pyll_upper99_8cl = value + sqrt((err_frac/sum({{ x }}, na.rm=TRUE)))*
                  (byars_upper(sum({{ x }}, na.rm=TRUE), conf2) - sum({{ x }}, na.rm=TRUE)) * multiplier,
                .groups = "keep") %>%
      select(-err_frac) %>%
      mutate(confidence = "95%, 99.8%",
             statistic = paste("dsr per",format(multiplier,scientific=F)),
             method = "Dobson PYLL variation")

    # remove DSR calculation for total counts < 10 - because numerator is years not deaths disclosure not applied
#    phe_dsr$value[phe_dsr$total_count             < 10] <- NA
#    phe_dsr$upper95_0cl[phe_dsr$total_count       < 10] <- NA
#    phe_dsr$lower95_0cl[phe_dsr$total_count       < 10] <- NA
#    phe_dsr$upper99_8cl[phe_dsr$total_count       < 10] <- NA
#    phe_dsr$lower99_8cl[phe_dsr$total_count       < 10] <- NA
#    phe_dsr$statistic[phe_dsr$total_count         < 10] <- "dsr NA for total count < 10"


    # drop fields not required based on value of type argument
    if (type == "lower") {
      phe_pyll <- phe_pyll %>%
        select(-total_count, -total_pop, -value, -upper95_0cl, -upper99_8cl,
               -confidence, -statistic, -method)
    } else if (type == "upper") {
      phe_pyll <- phe_pyll %>%
        select(-total_count, -total_pop, -value, -lower95_0cl, -lower99_8cl,
               -confidence, -statistic, -method)
    } else if (type == "value") {
      phe_pyll <- phe_pyll %>%
        select(-total_count, -total_pop, -lower95_0cl, -lower99_8cl, -upper95_0cl, -upper99_8cl,
               -confidence, -statistic, -method)
    } else if (type == "standard") {
      phe_pyll <- phe_pyll %>%
        select(-confidence, -statistic, -method)
    }

  } else {

    # scale confidence level if single value specified
    if (confidence >= 90) {
      confidence <- confidence/100
    }


    # calculate pyll with a single CI
    phe_pyll <- data %>%
      mutate(wt_rate = na.zero({{ x }}) *  stdpop_calc / ({{ n }}),
             sq_rate = na.zero({{ x }}) * (stdpop_calc / ({{ n }}))^2, na.rm=TRUE) %>%
      summarise(total_count = sum({{ x }},na.rm=TRUE),
                total_pop = sum({{ n }}),
                value = sum(wt_rate) / sum(stdpop_calc) * multiplier,
                err_frac = sum(sum(stdpop_calc)^2 * sum(x)*sum(leadj)^2),
                lowercl = value + sqrt((err_frac/sum({{ x }},na.rm=TRUE)))*(byars_lower(sum({{ x }},na.rm=TRUE),
                                                                                      confidence)-sum({{ x }},na.rm=TRUE)) * multiplier,
                uppercl = value + sqrt((err_frac/sum({{ x }},na.rm=TRUE)))*(byars_upper(sum({{ x }},na.rm=TRUE),
                                                                                      confidence)-sum({{ x }},na.rm=TRUE)) * multiplier,
                .groups = "keep") %>%
      select(-err_frac) %>%
      mutate(confidence = paste(confidence*100,"%",sep=""),
             statistic = paste("dsr per",format(multiplier,scientific=F)),
             method = "Dobson PYLL variation")

    # remove DSR calculation for total counts < 10
 #   phe_dsr$value[phe_dsr$total_count            < 10] <- NA
 #   phe_dsr$uppercl[phe_dsr$total_count          < 10] <- NA
 #   phe_dsr$lowercl[phe_dsr$total_count          < 10] <- NA
 #   phe_dsr$statistic[phe_dsr$total_count        < 10] <- "dsr NA for total count < 10"

    # drop fields not required based on value of type argument
    if (type == "lower") {
      phe_pyll <- phe_pyll %>%
        select(-total_count, -total_pop, -value, -uppercl, -confidence, -statistic, -method)
    } else if (type == "upper") {
      phe_pyll <- phe_pyll %>%
        select(-total_count, -total_pop, -value, -lowercl, -confidence, -statistic, -method)
    } else if (type == "value") {
      phe_pyll <- phe_pyll %>%
        select(-total_count, -total_pop, -lowercl, -uppercl, -confidence, -statistic, -method)
    } else if (type == "standard") {
      phe_pyll <- phe_pyll %>%
        select(-confidence, -statistic, -method)
    }

  }
  return(phe_pyll)

}
