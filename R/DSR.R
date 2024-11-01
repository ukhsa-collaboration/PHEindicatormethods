# -------------------------------------------------------------------------------------------------
#' dsr_inner
#'
#' Generates dsrs. All arguments are passed from the calculate_dsr wrapper
#' function with the exception of those defined below
#'
#' @param get_nonindependent_vardsr keep defaults for independent events. For
#'   non-independent events, set to TRUE on the first iteration through the
#'   function to output the variance to pass in to the second iteration.
#' @param use_nonindependent_vardsr keep defaults for independent events. For
#'   non-independent events, pass the unquoted column name that holds the event
#'   frequency variance to be passed in to the second iteration.
#'
#' @inheritParams calculate_dsr
#'
#' @return Returns a data frame of ID variables plus the dsr value, confidence
#'   limit & metadata fields as specified by the type argument. For
#'   non-independent events, returns just the ID variables and the dsr variance
#'   to be passed into the second iteration.
#'
#' @section Notes: This is an internal package function that is called by
#'   calculate_dsr.  It will run through once when events are independent. When
#'   events are non-independent, it will run through twice, calculating the
#'   variance for event frequencies on the first iteration and passing this
#'   value in to the function on the second iteration.
#'
#' @noRd
#'
# -------------------------------------------------------------------------------------------------

dsr_inner <- function(data,
                      x,
                      n,
                      stdpop,
                      type,
                      confidence,
                      multiplier,
                      get_nonindependent_vardsr = FALSE,
                      use_nonindependent_vardsr = NA_real_) {

  # validate arguments specific to dsr_inner function
  if (get_nonindependent_vardsr == TRUE & "custom_vardsr" %in% names(data)) {
    stop("cannot get nonindependent vardsr and use nonindependent vardsr in the same execution")
  }

  # scale and extract confidence values
  confidence[confidence >= 90] <- confidence[confidence >= 90] / 100
  conf1 <- confidence[1]
  conf2 <- confidence[2]

  # create dummy custom_vardsr column when not in use to prevent errors evaluating vardsr code
  if (!"custom_vardsr" %in% names(data)) {
    data <- data %>%
      mutate(custom_vardsr = NA_real_)
  }

  # calculate DSR and CIs
  dsrs <- data %>%
      mutate(wt_rate = na.zero(x) *  .data$stdpop / (n),
             sq_rate = na.zero(x) * (.data$stdpop / (n))^2, na.rm=TRUE) %>%
      summarise(total_count = sum(x, na.rm=TRUE),
                total_pop = sum(n),
                value = sum(.data$wt_rate) / sum(.data$stdpop) * multiplier,
                vardsr = case_when(
                  !is.na(unique(.data$custom_vardsr)) ~ unique(.data$custom_vardsr),
                  .default = 1/sum(.data$stdpop)^2 * sum(.data$sq_rate)),
                lowercl = .data$value + sqrt((.data$vardsr/sum(x, na.rm=TRUE)))*
                    (byars_lower(sum(x, na.rm=TRUE), conf1) - sum(x, na.rm=TRUE)) * multiplier,
                uppercl = .data$value + sqrt((.data$vardsr/sum(x, na.rm=TRUE)))*
                    (byars_upper(sum(x, na.rm=TRUE), conf1) - sum(x, na.rm=TRUE)) * multiplier,
                lower99_8cl = case_when(
                    is.na(conf2) ~ NA_real_,
                    .default = (.data$value + sqrt((.data$vardsr/sum(x, na.rm=TRUE)))*
                    (byars_lower(sum(x, na.rm=TRUE), min(conf2, 1, na.rm = TRUE)) - sum(x, na.rm=TRUE)) * multiplier)
                  ),
                upper99_8cl = case_when(
                    is.na(conf2) ~ NA_real_,
                    .default = (.data$value + sqrt((.data$vardsr/sum(x, na.rm=TRUE)))*
                    (byars_upper(sum(x, na.rm=TRUE), min(conf2, 1, na.rm = TRUE)) - sum(x, na.rm=TRUE)) * multiplier)
                 ),
                .groups = "keep") %>%
      mutate(confidence = paste0(confidence * 100, "%", collapse = ", "),
             statistic = paste("dsr per",format(multiplier, scientific=F)),
             method = "Dobson")


  # rename or drop confidence limits depending whether 1 or 2 CIs requested
  if (!is.na(conf2)) {
    names(dsrs)[names(dsrs) == "lowercl"] <- "lower95_0cl"
    names(dsrs)[names(dsrs) == "uppercl"] <- "upper95_0cl"
  } else {
   dsrs <- dsrs %>%
     select(!c("lower99_8cl", "upper99_8cl"))
  }


  # remove DSR calculation for total counts < 10 unless doing non-independent event CIs
  if (!get_nonindependent_vardsr) {
    dsrs <- dsrs %>%
      mutate(across(c("value", starts_with("upper"), starts_with("lower")),
                  function(x) if_else(.data$total_count < 10, NA_real_, x)),
             statistic = if_else(.data$total_count < 10,
                                 "dsr NA for total count < 10",
                                 .data$statistic))
  }

  # drop fields not required based on values of nonindependent_breakdowns and type arguments
  if (get_nonindependent_vardsr) {
    dsrs <- dsrs %>%
      select("vardsr")
  } else if (type == "lower") {
   dsrs <- dsrs %>%
      select(!c("total_count", "total_pop", "value", starts_with("upper"),
                "vardsr", "confidence", "statistic", "method"))
  } else if (type == "upper") {
      dsrs <- dsrs %>%
      select(!c("total_count", "total_pop", "value", starts_with("lower"),
                "vardsr", "confidence", "statistic", "method"))
  } else if (type == "value") {
      dsrs <- dsrs %>%
      select(!c("total_count", "total_pop", starts_with("lower"), starts_with("upper"),
                "vardsr", "confidence", "statistic", "method"))
  } else if (type == "standard") {
      dsrs <- dsrs %>%
      select(!c("vardsr", "confidence", "statistic", "method"))
  } else if (type == "full") {
    dsrs <- dsrs %>%
      select(!c("vardsr"))
  }

  return(dsrs)

}

# -------------------------------------------------------------------------------------------------
#' Calculate Directly Standardised Rates using calculate_dsr
#'
#' Calculates directly standardised rates with confidence limits using Byar's
#' method (1) with Dobson method adjustment (2) including option to further
#' adjust confidence for non-independent events.
#'
#' @param data data.frame containing the data to be standardised, pre-grouped if
#'   multiple DSRs required; unquoted string; no default
#' @param x field name from data containing the observed number of events for
#'   each standardisation category (eg ageband) within each grouping set (eg
#'   area); unquoted string; no default
#' @param n field name from data containing the populations for each
#'   standardisation category (eg ageband) within each grouping set (eg area);
#'   unquoted string; no default
#' @param stdpop field name from data containing the standard populations for
#'   each age band; unquoted string; default = esp2013
#' @param type defines the data and metadata columns to be included in output;
#'   can be "value", "lower", "upper", "standard" (for all data) or "full" (for
#'   all data and metadata); quoted string; default = "full"
#' @param confidence the required level of confidence expressed as a number
#'   between 0.9 and 1 or a number between 90 and 100 or can be a vector of 0.95
#'   and 0.998, for example, to output both 95 percent and 99.8 percent percent
#'   CIs; numeric; default 0.95
#' @param multiplier the multiplier used to express the final values (eg 100,000
#'   = rate per 100,000); numeric; default 100,000
#' @param independent_events whether events are independent. Set to TRUE for
#'   independent events. When set to FALSE an adjustment is made to the
#'   confidence intervals - to do this, the dataset provided must include event
#'   frequency breakdowns and column x is
#'   redefined as the number of unique people who experienced each frequency of
#'   event, rather than the total number of events. The function will then call
#'   the dsr_inner function twice - the first iteration will calculate and sum
#'   the variances for each event frequency, the second iteration will override
#'   the dsr variance calculation with the variance value obtained from the first
#'   iteration.
#' @param eventfreq field name from data containing the event frequencies. Only
#'   required when independentevents = FALSE; unquoted string; default NULL
#' @param ageband field name from data containing the age bands for
#'   standardisation. Only required when independentevents = FALSE; unquoted
#'   string; default NULL
#'
#' @return When type = "full", returns a tibble of total counts, total
#'   populations, directly standardised rates, lower confidence limits, upper
#'   confidence limits, confidence level, statistic and method for each grouping
#'   set. Use the type argument to limit the columns output.
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
#'     phe_dsr(obs, pop)
#'
#' ## calculate both 95% and 99.8% CIs in single execution
#' df %>%
#'     group_by(indicatorid, year, sex) %>%
#'     phe_dsr(obs, pop, confidence = c(0.95, 0.998))
#'
#' ## calculate DSRs for multiple grouping sets in single execution
#'
#' df %>%
#'     group_by(indicatorid, year, sex) %>%
#'     phe_dsr(obs, pop, type = "standard")
#'
#' @section Notes: User MUST ensure that x, n and stdpop vectors are all ordered
#'   by the same standardisation category values as records will be matched by
#'   position. \cr \cr For total counts >= 10 Byar's method (1) is applied using
#'   the internal byars_lower and byars_upper functions. When the total count is
#'   < 10 DSRs are not reliable and will therefore not be calculated except when
#'   calculating event frequency variance for non-independent events.
#'
#' @references (1) Breslow NE, Day NE. Statistical methods in cancer research,
#' volume II: The design and analysis of cohort studies. Lyon: International
#' Agency for Research on Cancer, World Health Organisation; 1987. \cr \cr (2)
#' Dobson A et al. Confidence intervals for weighted sums of Poisson parameters.
#' Stat Med 1991;10:457-62.
#'
#'
#' @family PHEindicatormethods package functions
# -------------------------------------------------------------------------------------------------

calculate_dsr <- function(data,
                          x,
                          n,
                          stdpop = esp2013,
                          type = "full",
                          confidence = 0.95,
                          multiplier = 100000,
                          independent_events = TRUE,
                          eventfreq = NULL,
                          ageband = NULL) {

  # check required arguments present
  if (missing(data)|missing(x)|missing(n)) {
      stop("function calculate_dsr requires at least 3 arguments: data, x, n")
  }

  # check same number of rows per group
  if (n_distinct(select(ungroup(count(data)),n)) != 1) {
      stop("data must contain the same number of rows for each group")
  }

  # check stdpop is valid and appended to data
  if (!deparse(substitute(stdpop)) %in% colnames(data)) {
      stop("stdpop is not a field name from data")
  }


  # hard-code field names
  data <- data %>%
    rename(x = {{ x }},
           n = {{ n }},
           stdpop = {{ stdpop }})


  # validate arguments
  if (any(pull(data, x) < 0, na.rm=TRUE)) {
      stop("numerators must all be greater than or equal to zero")
  } else if (any(pull(data, n) <= 0)) {
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


  if (independent_events) {
    # perform dsr using CI calculation for independent events
    dsrs <- dsr_inner(data       = data,
                      x          = x,
                      n          = n,
                      stdpop     = stdpop,
                      type       = type,
                      confidence = confidence,
                      multiplier = multiplier)

  } else {
    # perform dsr using CI calculation for non independent events

    # check that eventfrequency column is specified, exists
    if (missing(eventfreq)) {
      stop("function calculate_dsr requires an eventfreq column to be specified
            when independentevents is FALSE")
    } else if (!deparse(substitute(eventfreq)) %in% colnames(data)) {
      stop("eventfreq is not a field name from data")
    }

    # hard code eventfreq and ageband column names
    data <- data %>%
      rename(eventfreq = {{ eventfreq }},
             ageband = {{ ageband }})


    # check grouping variables and remove eventfrequency for use later
    grps <- group_vars(data)[!group_vars(data) %in% "eventfreq"]

    # get vardsrs for each event frequency and sum up
    freq_var <- data %>%
      dsr_inner(x          = x,
                n          = n,
                stdpop     = stdpop,
                type       = type,
                confidence = confidence,
                multiplier = multiplier,
                get_nonindependent_vardsr = TRUE) %>%
      mutate(freqvars = .data$vardsr * .data$eventfreq^2) %>%
      group_by(across(all_of(grps))) %>%
      summarise(custom_vardsr = sum(.data$freqvars))

    # summarise total events
    event_data <- data %>%
      mutate(events = .data$eventfreq * x) %>%
      group_by(across(all_of(grps)), ageband, n, stdpop) %>%
      summarise(x = sum(events), .groups = "drop")

    # calculate overall DSR passing in nonindependent variance
    dsrs <- event_data %>%
      left_join(freq_var, by = "indicator") %>%
      group_by(across(all_of(grps))) %>%
      dsr_inner(x          = x,
                n          = n,
                stdpop     = stdpop,
                type       = type,
                confidence = confidence,
                multiplier = multiplier,
                use_nonindependent_vardsr = custom_vardsr)
  }

  return(dsrs)

}
