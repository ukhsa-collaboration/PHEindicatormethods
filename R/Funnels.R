#' Calculate the funnel point value for a specific population based on a
#' population average value
#'
#' Returns a value equivalent to the higher/lower funnel plot point based on the
#' input population and probability
#'
#' @param p numeric (between 0 and 1); probability to calculate funnel plot
#'   point (will normally be either 0.975 or 0.999)
#' @param population numeric; the population for the area
#' @param average_proportion numeric; the average proportion for all the areas
#'   included in the funnel plot (the sum of the numerators divided by the sum
#'   of the denominators)
#' @param side string; "low" or "high" to determine which funnel to calculate
#' @param multiplier  numeric; the multiplier used to express the final values
#'   (eg 100 = percentage); default 100
#' @return returns a value equivalent to the specified funnel for the input
#'   population
#'
#' @author Sebastian Fox, \email{sebastian.fox@@phe.gov.uk}
#'
sigma_adjustment <- function(p, population, average_proportion, side, multiplier = 100) {
  first_part <- average_proportion * (population /
                             qnorm(p)^2 + 1)

  adj <- sqrt((-8 * average_proportion * (population /
                            qnorm(p)^2 + 1))^2 - 64 *
                (1 / qnorm(p)^2 + 1 / population) *
                average_proportion * (population *
                        (average_proportion * (population /
                                 qnorm(p)^2 + 2) - 1) +
                        qnorm(p)^2 *
                        (average_proportion - 1)))

  last_part <- (1 / qnorm(p)^2 + 1 /
                  population)

  if (side == "low") {
    adj_return <- (first_part - adj / 8) / last_part
  } else if (side == "high") {
    adj_return <- (first_part + adj / 8) / last_part
  }
  adj_return <- (adj_return /
                   population) * multiplier
  return(adj_return)
}

#' Calculate confidence intervals/control limits for funnel plots
#'
#' Calculates control limits adopting a consistent method as per the PHE
#' Fingertips Technical Guidance: https://fingertips.phe.org.uk/profile/guidance
#'
#' @param data a data.frame containing the data to calculate control limits for;
#'   unquoted string; no default
#'
#' @param numerator field name from data containing the observed numbers of cases in the
#'   sample meeting the required condition (the numerator for the CLs); unquoted
#'   string; no default
#' @param denominator field name from data containing the population(s) in the
#'   sample (the denominator for the CLs); unquoted string; no default
#' @param type defines the data and metadata columns to be included in output;
#'   "standard" (for all data) or "full" (for all data and metadata); quoted
#'   string; default = "full"
#' @param multiplier the multiplier used to express the final values (eg 100 =
#'   percentage); numeric; default 100
#' @param statistic string; type of statistic to inform funnel calculations.
#'   Acceptable values are "proportion", "ratio" or "rate"
#' @param ratio_type if statistic is "ratio" the user can specify either "count"
#'   or "isr" (indirectly standardised rate) here as a string
#' @param rate field name from data containing the rate data when creating
#'   funnels for a Directly Standardised Rate; unquoted string; no default
#' @param rate_type string; one of "dsr" or "crude"; when statistic is rate this
#'   argument is required
#' @param years_of_data numeric; number of years the data represents; this is
#'   required for statistic = "rate"
#' @return returns the original data.frame with the following appended: lower
#'   0.025 limit, upper 0.025 limit, lower 0.001 limit, upper 0.001 limit and
#'   baseline average
#'
#' @import dplyr
#' @importFrom rlang := .data
#'
#' @examples
#' library(dplyr)
#' set.seed(123)
#' df <- data.frame(obs = sample(200, 19 * 2 * 5 * 4, replace = TRUE),
#'                  pop = sample(10000:20000, 19 * 2 * 5 * 4, replace = TRUE))
#' df %>%
#'     phe_funnels(obs, pop)
#'
#' @export
#' @author Matthew Francis, \email{Matthew.Francis@@phe.gov.uk}
#'
#' @family PHEindicatormethods package functions

# Generate a dataframe of the confidence limits for plotting
# NB -this does not alter the original dataset but generates a dataframe of 100 records for
# plotting the control limits

phe_funnels <- function(data, numerator, denominator, rate,
                        type = "full",
                        multiplier = 100, statistic = "proportion",
                        ratio_type = NULL, rate_type = NULL,
                        years_of_data = NULL) {

  # check required arguments present
  if (statistic == "rate") {
    if (missing(data) | missing(numerator) | missing(rate)) {
      stop("at least 3 arguments are required for rates: data, numerator, rate")
    } else if (any(pull(data, {{ numerator }}) == 0) & missing(denominator)) {
      stop("for rates, where there are 0 events for a record, the denominator field needs to be provided using the denominator argument")
    } else if (is.null(multiplier)) {
      stop("for rates, a multiplier is required to test the significance of the points")
    }

    rate_type <- match.arg(rate_type, c("dsr", "crude"))

  } else if (statistic %in% c("proportion", "ratio")) {
    if (missing(data) | missing(numerator) | missing(denominator)) {
      stop("at least 3 arguments are required for ratios and proportions: data, numerator, denominator")
    }
  }


  # check string inputs
  type <- match.arg(type, c("full", "standard"))
  statistic <- match.arg(statistic, c("proportion", "ratio", "rate"))

  if (statistic == "ratio") ratio_type <- match.arg(ratio_type, c("count", "isr"))
  if (statistic == "rate") {
    rate_type <- match.arg(rate_type, c("dsr", "crude"))
    if (is.null(years_of_data)) stop("years_of_data is required when statstic is 'rate'")
  }


  if (statistic == "rate") {
    rate_type <- match.arg(rate_type, c("dsr", "crude"))
    if (rate_type == "dsr") {
      data <- data %>%
        mutate(
          {{ rate }} := as.numeric({{ rate }}),
          denominator_derived = case_when(
            {{ numerator }} == 0 ~ NA_real_,
            TRUE ~ multiplier * {{ numerator }} / {{ rate }}
          )
        )
    } else if (rate_type == "crude") {
      if (missing(denominator)) {
        data <- data %>%
          mutate(
            denominator_derived = multiplier * {{ numerator }} / {{ rate }}
          )

      } else {
        data <- data %>%
          mutate(
            denominator_derived = {{ denominator }}
          )

      }
      data <- data %>%
        mutate(
          {{ rate }} := as.numeric({{ rate }})
        )
    }
  } else if (statistic %in% c("proportion", "ratio")) {
    data <- data %>%
      mutate(denominator_derived = {{ denominator }})
  }
  # aggregated data
  summaries <- data %>%
    summarise(
      av = sum({{ numerator }}) / sum(.data$denominator_derived, na.rm = TRUE),
      min_denominator = min(.data$denominator_derived, na.rm = TRUE),
      max_denominator = max(.data$denominator_derived, na.rm = TRUE)
    )

  av <- summaries$av
  min_denominator <- summaries$min_denominator
  max_denominator <- summaries$max_denominator

  # function that rounds down to the desired significance level to make the axis display neat
  signif.floor <- function(x, percentage_down = 0.95) {
    n <- nchar(floor(x * percentage_down)) - 1
    y <- floor(x * percentage_down / 10^n) * 10^n

    # handle the x = 0 case
    y[x == 0] <- 0
    y
  }

  # function that rounds up to the desired significance level to make the axis display neat
  signif.ceiling <- function(x, percentage_up = 1.05) {
    n <- nchar(ceiling(x * percentage_up)) - 2
    y <- ceiling(x * percentage_up / 10^n) * 10^n

    # handle the x = 0 case
    y[x == 0] <- 0
    y
  }

  if (max_denominator > 2 * min_denominator) {
    axis_minimum <- 0
  } else {
    if (statistic == "rate") {
      axis_minimum <- signif.floor(min_denominator / years_of_data) *
        years_of_data

    } else if (statistic %in% c("proportion", "ratio")) {
      axis_minimum <- signif.floor(min_denominator)

    }
  }

  if (statistic == "rate") {
    axis_maximum <- signif.ceiling(max_denominator / years_of_data) *
      years_of_data
  } else if (statistic %in% c("proportion", "ratio")) {
    axis_maximum <- signif.ceiling(max_denominator)
  }


  if (statistic == "proportion") {
    col_header <- "Population"
  } else if (statistic == "ratio") {
    col_header <- "Observed_events"
  } else if (statistic == "rate") {
    col_header <- "Events"
    axis_minimum <- floor(axis_minimum * av)
    axis_maximum <- ceiling(axis_maximum * av)

  }

  first_col <- max(1, axis_minimum)
  for (j in 2:100) {
    if (statistic %in% c("proportion", "rate")) {
      offset <- j
    } else if (statistic == "ratio") {
      offset <- j - 1
    }
    first_col[j] <- max(round((axis_maximum / first_col[j - 1])^(1 / (101 - offset)) *
                                first_col[j - 1]),
                        first_col[j - 1] + 1)
  }

  t <- tibble(!! rlang::sym(col_header) := first_col)

  if (statistic == "proportion") {
    t <- t %>%
      group_by(.data$Population) %>%
      mutate(
        lower_2s_limit = max(0,
                             sigma_adjustment(0.975, .data$Population, av, "low", multiplier)),
        upper_2s_limit = min(100,
                             sigma_adjustment(0.975, .data$Population, av, "high", multiplier)),
        lower_3s_limit = max(0,
                             sigma_adjustment(0.999, .data$Population, av, "low", multiplier)),
        upper_3s_limit = min(100,
                             sigma_adjustment(0.999, .data$Population, av, "high", multiplier)),
        baseline = av * multiplier
      ) %>%
      ungroup()
  } else if (statistic == "ratio") {
    t <- t %>%
      group_by(.data$Observed_events) %>%
      mutate(
        lower_2s_exp_events = poisson_funnel(.data$Observed_events, 0.025, "high"),
        lower_2s_limit = .data$Observed_events / .data$lower_2s_exp_events,
        upper_2s_exp_events = poisson_funnel(.data$Observed_events, 0.025, "low"),
        upper_2s_limit = .data$Observed_events / .data$upper_2s_exp_events,
        lower_3s_exp_events = poisson_funnel(.data$Observed_events, 0.001, "high"),
        lower_3s_limit = .data$Observed_events / .data$lower_3s_exp_events,
        upper_3s_exp_events = poisson_funnel(.data$Observed_events, 0.001, "low"),
        upper_3s_limit = .data$Observed_events / .data$upper_3s_exp_events,
      ) %>%
      ungroup()
    if (ratio_type == "count") {
      t <- t %>%
        mutate(across(ends_with("limit"),
                      function(x) x - 1))
    } else if (ratio_type == "isr") {
      t <- t %>%
        mutate(across(ends_with("limit"),
                      function(x) x * 100))

    }
  } else if (statistic == "rate") {

    t <- t %>%
      group_by(.data$Events) %>%
      mutate(
        lower_2s_population_1_year = poisson_funnel(.data$Events, 0.025, "high") / av,
        lower_2s_limit = .data$Events / .data$lower_2s_population_1_year,
        upper_2s_population_1_year = poisson_funnel(.data$Events, 0.025, "low") / av,
        upper_2s_limit = .data$Events / .data$upper_2s_population_1_year,
        lower_3s_population_1_year = poisson_funnel(.data$Events, 0.001, "high") / av,
        lower_3s_limit = .data$Events / .data$lower_3s_population_1_year,
        upper_3s_population_1_year = poisson_funnel(.data$Events, 0.001, "low") / av,
        upper_3s_limit = .data$Events / .data$upper_3s_population_1_year,
        baseline = av * multiplier,
        across(ends_with("limit"),
               function(x) x * multiplier),
        across(ends_with("1_year"),
               function(x) x / years_of_data)
        ) %>%
      ungroup()
  }

  if (type == "full") {
    t$statistic <- statistic
    if (statistic == "ratio") {
      t <- t %>%
        mutate(statistic = paste0(
          statistic, " (", ratio_type, ")"
        ))
    } else if (statistic == "rate") {
      t <- t %>%
        mutate(statistic = paste0(
          statistic, " (", rate_type, " per ", format(multiplier, big.mark = ",", scientific = FALSE), ")"
        ))
    }
  }


  return(t)
}


#' Identifies level of outlier based on funnel plot methodology
#'
#' Determines whether records are outliers, and the level they are outliers,
#' among a dataset of numerators and denominators. This follows the same
#' methodology as that published on the PHE Fingertips Technical Guidance page:
#' https://fingertips.phe.org.uk/profile/guidance
#'
#' @param data a data.frame containing the data to calculate control limits for,
#' @param numerator field name from data containing the observed numbers of cases in the
#'   sample meeting the required condition (the numerator for the CLs); unquoted
#'   string; no default
#' @param denominator field name from data containing the population(s) in the
#'   sample (the denominator for the CLs); unquoted string; no default
#' @param rate field name from data containing the rate data when creating
#'   funnels for a Directly Standardised Rate; unquoted string; no default
#' @param rate_type string; one of "dsr" or "crude"; when statistic is rate this
#'   argument is required
#' @param multiplier numeric; the multiplier that the rate is normalised
#'   with (ie, per 100,000); only required when statistic = "rate
#' @inheritParams phe_funnels
#'
#' @return returns the original data.frame with the significance level appended
#'
#' @import dplyr
#'
#' @examples
#' library(dplyr)
#' df <- data.frame(
#'   Area = c("A", "B", "C", "D"),
#'   numerator = c(10232, 12321, 15123, 13213),
#'   denominator = c(15232, 16123, 17932, 18475)
#' )
#' df %>%
#'   phe_funnel_significance(numerator, denominator)
#'
#' @export
#'
#' @family PHEindicatormethods package functions
#' @author Matthew Francis, \email{Matthew.Francis@@phe.gov.uk}

phe_funnel_significance <- function(data, numerator, denominator, rate,
                                    statistic = "proportion", rate_type = NA,
                                    multiplier = NULL) {

  # check required arguments present
  if (statistic == "rate") {
    if (missing(data) | missing(numerator) | missing(rate)) {
      stop("at least 3 arguments are required for rates: data, numerator, rate")
    } else if (any(pull(data, {{ numerator }}) == 0) & missing(denominator)) {
      stop("for rates, where there are 0 events for a record, the denominator field needs to be provided using the denominator argument")
    } else if (is.null(multiplier)) {
      stop("for rates, a multiplier is required to test the significance of the points")
    }

    rate_type <- match.arg(rate_type, c("dsr", "crude"))

  } else if (statistic %in% c("proportion", "ratio")) {
    if (missing(data) | missing(numerator) | missing(denominator)) {
      stop("at least 3 arguments are required for ratios and proportions: data, numerator, denominator")
    }
  }

  # validate arguments
  if (any(pull(data, {{ numerator }}) < 0, na.rm = TRUE)) {
    stop("numerators must be greater than or equal to zero")
  } else if (any(pull(data, {{ denominator }}) <= 0, na.rm = TRUE)) {
    stop("denominators must be greater than zero")
  }

  if (statistic == "proportion") {
    if (any(pull(data, {{ numerator }}) > pull(data, {{ denominator }}), na.rm = TRUE)) {
      stop("numerators must be less than or equal to denominator for a proportion statistic")
    }
  }


  average <- sum(pull(data, {{ numerator }})) /
    sum(pull(data, {{ denominator }}))

  if (statistic == "proportion") {
    dummy_multiplier <- 1
    significance <- data %>%
      mutate(significance = case_when(
        {{ numerator }} / {{ denominator }} < sigma_adjustment(p = 0.999,
                                                       population = {{ denominator }},
                                                       average_proportion = average,
                                                       side = "low",
                                                       multiplier = dummy_multiplier) ~ "Low (0.001)",
        {{ numerator }} / {{ denominator }} < sigma_adjustment(p = 0.975,
                                                       population = {{ denominator }},
                                                       average_proportion = average,
                                                       side = "low",
                                                       multiplier = dummy_multiplier) ~ "Low (0.025)",
        {{ numerator }} / {{ denominator }} > sigma_adjustment(p = 0.999,
                                                       population = {{ denominator }},
                                                       average_proportion = average,
                                                       side = "high",
                                                       multiplier = dummy_multiplier) ~ "High (0.001)",
        {{ numerator }} / {{ denominator }} > sigma_adjustment(p = 0.975,
                                                       population = {{ denominator }},
                                                       average_proportion = average,
                                                       side = "high",
                                                       multiplier = dummy_multiplier) ~ "High (0.025)",
        TRUE ~ "Not significant"
      ))

        significance_levels <- c("High (0.001)",
                             "High (0.025)",
                             "Low (0.001)",
                             "Low (0.025)",
                             "Not significant")

  } else if (statistic == "ratio") {
    significance <- data %>%
      rowwise() %>%
      mutate(significance = case_when(
        1 < funnel_ratio_significance({{ numerator }}, {{ denominator }}, 0.998, "low") ~ "High (0.001)",
        1 < funnel_ratio_significance({{ numerator }}, {{ denominator }}, 0.95, "low") ~ "High (0.025)",
        1 > funnel_ratio_significance({{ numerator }}, {{ denominator }}, 0.998, "high") ~ "Low (0.001)",
        1 > funnel_ratio_significance({{ numerator }}, {{ denominator }}, 0.95, "high") ~ "Low (0.025)",
        TRUE ~ "Not significant"
      )) %>%
      ungroup()

    significance_levels <- c("High (0.001)",
                             "High (0.025)",
                             "Low (0.001)",
                             "Low (0.025)",
                             "Not significant")

  } else if (statistic == "rate") {
    # calculate approximate weighted average
    if (rate_type == "dsr") {
      data <- data %>%
        mutate(
          derived_denominator = case_when(
            {{ numerator }} == 0 ~ NA_real_,
            TRUE ~ {{ numerator }} / {{ rate }} * multiplier
          )
        )
    } else if (rate_type == "crude") {
      data <- data %>%
        mutate(
          derived_denominator = case_when(
            {{ numerator }} == 0 ~ {{ denominator }},
            TRUE ~ {{ numerator }} / {{ rate }} * multiplier
          ))

    }

    weighted_average <- sum(pull(data, {{ numerator }})) /
      sum(pull(data, .data$derived_denominator), na.rm = TRUE)

    significance <- data %>%
      rowwise() %>%
      mutate(significance = case_when(
        weighted_average < funnel_ratio_significance({{ numerator }}, .data$derived_denominator, 0.998, "low") ~ "High (0.001)",
        weighted_average < funnel_ratio_significance({{ numerator }}, .data$derived_denominator, 0.95, "low") ~ "High (0.025)",
        weighted_average > funnel_ratio_significance({{ numerator }}, .data$derived_denominator, 0.998, "high") ~ "Low (0.001)",
        weighted_average > funnel_ratio_significance({{ numerator }}, .data$derived_denominator, 0.95, "high") ~ "Low (0.025)",
        TRUE ~ "Not significant"
      )) %>%
      ungroup() %>%
      select(-.data$derived_denominator)
    if (rate_type == "dsr") {
      significance <- significance %>%
        mutate(
          significance = case_when(
            {{ numerator }} < 10 ~ "Not applicable for events less than 10 for dsrs",
            TRUE ~ significance
          )
        )
    }

    significance_levels <- c("High (0.001)",
                             "High (0.025)",
                             "Low (0.001)",
                             "Low (0.025)",
                             "Not significant",
                             "Not applicable for events less than 10 for dsrs")

  }

  significance <- significance %>%
    mutate(significance = factor(significance,
                                 levels = significance_levels))
  return(significance)
}

#' Derive rate and annual population values for charting based. Process removes
#' rates where the rate type is dsr and the number of observed events are below
#' 10.
#'
#'
#' @inheritParams phe_funnels
#' @export
#' @return returns the same table as provided with two additional fields. First
#'   will have the same name as the rate field, with the suffix "_chart", the
#'   second will be called denominator_derived
#'
#' @author Sebastian Fox, \email{sebastian.fox@@phe.gov.uk}
#'
phe_funnel_convert_points <- function(data, numerator, denominator, rate,
                                      rate_type, years_of_data, multiplier = NULL) {

  # check required arguments present
  if (missing(data) | missing(numerator) | missing(rate)) {
    stop("at least 3 arguments are required for rates: data, numerator, rate")
  } else if (any(pull(data, {{ numerator }}) == 0) & missing(denominator)) {
    stop("for rates, where there are 0 events for a record, the denominator field needs to be provided using the denominator argument")
  } else if (is.null(multiplier)) {
    stop("a multiplier is required to convert the input data")
  }


  rate_type <- match.arg(rate_type, c("dsr", "crude"))
  if (rate_type == "dsr") {
    data <- data %>%
      mutate(
        "{{ rate }}_chart" := case_when(
          {{ numerator }} < 10 ~ NA_real_,
          TRUE ~ as.numeric({{ rate }})
        ),
        denominator_derived = case_when(
          {{ numerator }} < 10 ~ NA_real_,
          TRUE ~ (multiplier * {{ numerator }} / {{ rate }}) / years_of_data
        )
      )
  } else if (rate_type == "crude") {
    data <- data %>%
      mutate(
        "{{ rate }}_chart" := as.numeric({{ rate }}))

    if (missing(denominator)) {
      data <- data %>%
        mutate(
          denominator_derived = (multiplier * {{ numerator }} / {{ rate }}) / years_of_data
        )
    } else {
      data <- data %>%
        mutate(
          denominator_derived = case_when(
            {{ numerator }} == 0 ~ {{ denominator }} / years_of_data,
            TRUE ~ (multiplier * {{ numerator }} / {{ rate }}) / years_of_data
      ))
    }
  }

  return(data)

}
