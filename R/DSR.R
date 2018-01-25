# -------------------------------------------------------------------------------------------------
#' DSR
#'
#' Calculates a directly standardised rate with confidence limits using Dobson method.
#'
#' @param data data.frame containing the data to be standarised, pre-grouped if multiple DSRs required; unquoted string; no default
#' @param x field name from data containing the observed number of events for each standardisation category (eg ageband) within each grouping set (eg area);
#'          unquoted string; no default
#' @param n field name from data containing the populations for each standardisation category (eg ageband) within each grouping set (eg area);
#'          unquoted string; no default
#' @param stdpop the standard populations for each standardisation category (eg age band); unquoted numeric vector; no default
#' @param type type of output; can be "value", "lower", "upper", "standard" (for all 3 previous fields to be added to your output) or "full"; string; default combined
#' @param confidence the required level of confidence expressed as a number between 0.9 and 1
#'                   or 90 and 100; numeric; default 0.95
#' @param multiplier the multiplier used to express the final values (eg 100,000 = rate per 100,000,
#'                   100 = percentage); numeric; default 100,000
#'
#' @return When type = "full" returns a data.frame of numerator, denominator, directly standardised rate,
#'         lower and upper confidence limits and method
#' @importFrom rlang sym quo_name
#'
#' @examples
#' library(dplyr)
#' df <- data.frame(indicatorid = rep(c(1234, 5678, 91011, 121314), each = 19 * 2 * 5),
#'                  year = rep(2006:2010, each = 19 * 2),
#'                  sex = rep(rep(c("Male", "Female"), each = 19), 5),
#'                  obs = sample(200, 19 * 2 * 5 * 4, replace = TRUE),
#'                  pop = sample(10000:20000, 19 * 2 * 5 * 4, replace = TRUE))
#' df %>%
#'     group_by(indicatorid, year, sex) %>%
#'     phe_dsr(obs, pop, esp2013)
#'
#' ## OR
#'
#' df %>%
#'     group_by(indicatorid, year, sex) %>%
#'     phe_dsr(obs, pop, esp2013, "full")
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

# define the DSR function
phe_dsr <- function(data, x, n, stdpop, type = "standard", confidence = 0.95, multiplier = 100000) {

# check required arguments present
  if (missing(data)|missing(x)|missing(n)|missing(stdpop)) {
      stop("function phe_dsr requires at least 4 arguments: data, x, n, stdpop")
  }

# apply quotes
  x <- enquo(x)
  n <- enquo(n)
  enquostdpop <- enquo(stdpop)

# validate arguments
  if (any(pull(data, !!x) < 0)) {
      stop("numerators must all be greater than or equal to zero")
  } else if (any(pull(data, !!n) <= 0)) {
      stop("denominators must all be greater than zero")
  } else if ((confidence<0.9)|(confidence >1 & confidence <90)|(confidence > 100)) {
      stop("confidence level must be between 90 and 100 or between 0.9 and 1")
  } else if (!(type %in% c("value", "lower", "upper", "combined", "full"))) {
      stop("type must be one of value, lower, upper, standard or full")
  } else if (n_distinct(select(summarise(data,n=n()),n)) != 1) {
    stop("data must contain the same number of rows for each group")
  } else if (!exists("stdpop", where=data)) {
      if (pull(slice(select(summarise(data,n=n()),n),1)) != length(stdpop)) {
        stop("stdpop length must equal number of rows in each group within data")
    }
  }

# scale confidence level
  if (confidence >= 90) {
    confidence <- confidence/100
  }


# calculate DSR and CIs
  phe_dsr <- data %>%
    mutate(wt_rate = (!!x) * (!!enquostdpop) / (!!n),
           sq_rate = (!!x) * ((!!enquostdpop)/(!!n))^2) %>%
    summarise(total_count = sum(!!x),
              total_pop = sum(!!n),
              dsr = sum(wt_rate) / sum((!!enquostdpop)) * multiplier,
              vardsr = 1/sum(!!enquostdpop)^2 * sum(sq_rate),
              lowercl = dsr + sqrt((vardsr/sum(!!x)))*(byars_lower(sum(!!x),confidence)-sum(!!x)) * multiplier,
              uppercl = dsr + sqrt((vardsr/sum(!!x)))*(byars_upper(sum(!!x),confidence)-sum(!!x)) * multiplier) %>%
    select(-vardsr) %>%
    mutate(confidence = paste(confidence*100,"%"),
           method = if_else(total_count < 10,"NA","Dobson"))

  phe_dsr$dsr[phe_dsr$total_count < 10]        <- "NA - total count is < 10"
  phe_dsr$uppercl[phe_dsr$total_count < 10]    <- "NA - total count is < 10"
  phe_dsr$lowercl[phe_dsr$total_count < 10]    <- "NA - total count is < 10"
  phe_dsr$confidence[phe_dsr$total_count < 10] <- "NA - total count is < 10"
  phe_dsr$method[phe_dsr$total_count < 10]     <- "NA - total count is < 10"


  if (type == "lower") {
    phe_dsr <- phe_dsr %>%
      select(-total_count, -total_pop, -dsr, -uppercl, -confidence, -method)
  } else if (type == "upper") {
    phe_dsr <- phe_dsr %>%
      select(-total_count, -total_pop, -dsr, -lowercl, -confidence, -method)
  } else if (type == "value") {
    phe_dsr <- phe_dsr %>%
      select(-total_count, -total_pop, -lowercl, -uppercl, -confidence, -method)
  } else if (type == "standard") {
    phe_dsr <- phe_dsr %>%
      select(-total_count, -total_pop, -confidence, -method)
  }
  return(phe_dsr)

}
