# -------------------------------------------------------------------------------------------------
#' phe_dsr
#'
#' Calculates directly standardised rates with confidence limits using Byar's method [1] with Dobson method adjustment [2].
#'
#' @param data data.frame containing the data to be standardised, pre-grouped if multiple DSRs required; unquoted string; no default
#' @param x field name from data containing the observed number of events for each standardisation category (eg ageband) within each grouping set (eg area);
#'          unquoted string; no default
#' @param n field name from data containing the populations for each standardisation category (eg ageband) within each grouping set (eg area);
#'          unquoted string; no default
#' @param stdpop the standard populations for each standardisation category (eg age band);
#'               unquoted string referencing a numeric vector or field name from data depending on value of stdpoptype; default = esp2013
#' @param stdpoptype whether the stdpop has been specified as a vector or a field name from data;
#'                   quoted string "field" or "vector"; default = "vector"
#' @param type type of output; can be "value", "lower", "upper", "standard" (for all 3 previous fields) or "full"; quoted string; default standard
#' @param confidence the required level of confidence expressed as a number between 0.9 and 1
#'                   or 90 and 100; numeric; default 0.95
#' @param multiplier the multiplier used to express the final values (eg 100,000 = rate per 100,000); numeric; default 100,000
#'
#' @return When type = "full", returns a tibble of total counts, total populations, directly standardised rates,
#'  lower confidence limits, upper confidence limits, confidence level, statistic and method for each grouping set
#'
#' @importFrom rlang sym quo_name
#'
#' @import dplyr
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
#' df %>%
#'     group_by(indicatorid, year, sex) %>%
#'     phe_dsr(obs, pop)
#'
#' ## OR
#'
#' df %>%
#'     group_by(indicatorid, year, sex) %>%
#'     phe_dsr(obs, pop, type = "full")
#'
#' @section Notes: User MUST ensure that x, n and stdpop vectors are all ordered by
#' the same standardisation category values as records will be matched by position. \cr \cr
#' For total counts >= 10 Byar's method [1] is applied using the \code{\link{byars_lower}}
#' and \code{\link{byars_upper}} functions.  When the total count is < 10 DSRs are not reliable and will
#' therefore not be calculated.
#'
#' @references
#' [1] Breslow NE, Day NE. Statistical methods in cancer research,
#'  volume II: The design and analysis of cohort studies. Lyon: International
#'  Agency for Research on Cancer, World Health Organisation; 1987. \cr \cr
#' [2] Dobson A et al. Confidence intervals for weighted sums of Poisson parameters. Stat Med 1991;10:457-62.
#'
#' @export
#'
#' @family PHEindicatormethods package functions
# -------------------------------------------------------------------------------------------------

# define the DSR function using Dobson method
phe_dsr <- function(data, x, n, stdpop = esp2013, stdpoptype = "vector", type = "standard", confidence = 0.95, multiplier = 100000) {

  # check required arguments present
  if (missing(data)|missing(x)|missing(n)) {
      stop("function phe_dsr requires at least 3 arguments: data, x, n")
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
#     data <- bind_cols(data,stdpop_calc = rep(stdpop,times=nrow(summarise(data,n=n())))) - below alternative simpler
      data <- mutate(data,stdpop_calc = stdpop)
     } else if (stdpoptype == "field") {
      enquostdpop <- enquo(stdpop)
     if (deparse(substitute(stdpop)) %in% colnames(data)) {
       data <- mutate(data,stdpop_calc = !!enquostdpop)
     } else stop("stdpop is not a field name from data")
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

# calculate DSR and CIs
  phe_dsr <- data %>%
    mutate(wt_rate = (!!x) * stdpop_calc / (!!n),
           sq_rate = (!!x) * (stdpop_calc/(!!n))^2) %>%
    summarise(total_count = sum(!!x),
              total_pop = sum(!!n),
              value = sum(wt_rate) / sum(stdpop_calc) * multiplier,
              vardsr = 1/sum(stdpop_calc)^2 * sum(sq_rate),
              lowercl = value + sqrt((vardsr/sum(!!x)))*(byars_lower(sum(!!x),confidence)-sum(!!x)) * multiplier,
              uppercl = value + sqrt((vardsr/sum(!!x)))*(byars_upper(sum(!!x),confidence)-sum(!!x)) * multiplier) %>%
    select(-vardsr) %>%
    mutate(confidence = paste(confidence*100,"%",sep=""),
           statistic = paste("dsr per",format(multiplier,scientific=F)),
           method = "Dobson")

  phe_dsr$value[phe_dsr$total_count < 10]    <- NA
  phe_dsr$uppercl[phe_dsr$total_count < 10]  <- NA
  phe_dsr$lowercl[phe_dsr$total_count < 10]  <- NA
  phe_dsr$statistic[phe_dsr$total_count <10] <- "dsr NA for total count < 10"

  if (type == "lower") {
    phe_dsr <- phe_dsr %>%
      select(-total_count, -total_pop, -value, -uppercl, -confidence, -statistic, -method)
  } else if (type == "upper") {
    phe_dsr <- phe_dsr %>%
      select(-total_count, -total_pop, -value, -lowercl, -confidence, -statistic, -method)
  } else if (type == "value") {
    phe_dsr <- phe_dsr %>%
      select(-total_count, -total_pop, -lowercl, -uppercl, -confidence, -statistic, -method)
  } else if (type == "standard") {
    phe_dsr <- phe_dsr %>%
      select(-total_count, -total_pop, -confidence, -statistic, -method)
  }
  return(phe_dsr)

}
