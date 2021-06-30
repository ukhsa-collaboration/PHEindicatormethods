# -------------------------------------------------------------------------------------------------
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
# -------------------------------------------------------------------------------------------------
sigma_adjustment <- function(p, population, average_proportion, side, multiplier) {
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




# -------------------------------------------------------------------------------------------------
#' Calculate confidence intervals/control limits for funnel plots
#'
#' Calculates control limits adopting a consistent method as per the PHE
#' Fingertips Technical Guidance: https://fingertips.phe.org.uk/profile/guidance
#'
#' @param data a data.frame containing the data to calculate control limits for;
#'   unquoted string; no default
#'
#' @param x field name from data containing the observed numbers of cases in the
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
#'   Currently only accepts "proportion"
#' @param ratio_type if statistic is "ratio" the user can specify either "count"
#'   or "isr" (indirectly standardised rate) here as a string
#' @return returns the original data.frame with the following appended: lower
#'   0.025 limit, upper 0.025 limit, lower 0.001 limit, upper 0.001 limit and
#'   baseline average
#'
#' @import dplyr
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
# -------------------------------------------------------------------------------------------------

# Generate a dataframe of the confidence limits for plotting
# NB -this does not alter the original dataset but generates a dataframe of 100 records for
# plotting the control limits

phe_funnels <- function(data, x, denominator, type = "full",
                        multiplier = 100, statistic = "proportion",
                        ratio_type = NA) {

  # check required arguments present
  if (missing(data) | missing(x) | missing(denominator)) {
    stop("function phe_funnels requires at least 3 arguments: data, x, n")
  }

  # check string inputs
  type <- match.arg(type, c("full", "standard"))
  statistic <- match.arg(statistic, c("proportion", "ratio"))

  if (statistic == "ratio") ratio_type <- match.arg(ratio_type, c("count", "isr"))

  # aggregated data
  summaries <- data %>%
    summarise(
      av = sum({{ x }}) / sum({{ denominator }}),
      min_denominator = min({{ denominator }}),
      max_denominator = max({{ denominator }})
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
    axis_minimum <- signif.floor(min_denominator)
  }

  axis_maximum <- signif.ceiling(max_denominator)

 if (statistic == "proportion") {
    col_header <- "Population"
 } else if (statistic == "ratio") {
    col_header <- "Observed_events"
  }

  first_col <- max(1, axis_minimum)
  for (j in 2:100) {
    if (statistic == "proportion") {
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
  }

  if (type == "full") {
    t$statistic <- statistic
    if (statistic == "ratio") {
      t <- t %>%
        mutate(statistic = paste0(
          statistic, " (", ratio_type, ")"
        ))
    }
  }


  return(t)
}


# -------------------------------------------------------------------------------------------------
#' Identifies level of outlier based on funnel plot methodology
#'
#' Determines whether records are outliers, and the level they are outliers,
#' among a dataset of numerators and denominators. This follows the same
#' methodology as that published on the PHE Fingertips Technical Guidance page:
#' https://fingertips.phe.org.uk/profile/guidance
#'
#' @param data a data.frame containing the data to calculate control limits for,
#' @param x field name from data containing the observed numbers of cases in the
#'   sample meeting the required condition (the numerator for the CLs); unquoted
#'   string; no default
#' @param denominator field name from data containing the population(s) in the
#'   sample (the denominator for the CLs); unquoted string; no default
#' @param multiplier the multiplier used to express the final values (eg 100 =
#'   percentage); numeric; default 100
#' @inheritParams phe_funnels
#'
#' @return returns the original data.frame with the significance level appended
#'
#' @import dplyr
#' @importFrom rlang sym quo_name :=
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
# -------------------------------------------------------------------------------------------------
phe_funnel_significance <- function(data, x, denominator,
                                    multiplier = 100, statistic = "proportion") {

  # check required arguments present
  if (missing(data) | missing(x) | missing(denominator)) {
    stop("function phe_funnel_significance requires at least 3 arguments: data, x, n")
  }

  # validate arguments
  if (any(pull(data, {{ x }}) < 0, na.rm = TRUE)) {
    stop("numerators must be greater than or equal to zero")
  } else if (any(pull(data, {{ denominator }}) <= 0, na.rm = TRUE)) {
    stop("denominators must be greater than zero")
  } else if (any(pull(data, {{ x }}) > pull(data, {{ denominator }}), na.rm = TRUE)) {
    stop("numerators must be less than or equal to denominator for a proportion statistic")
  }

  average <- sum(pull(data, {{ x }})) /
    sum(pull(data, {{ denominator }}))

  significance <- data %>%
    mutate(significance = case_when(
      multiplier * {{ x }} / {{ denominator }} < sigma_adjustment(p = 0.999,
                                                                  population = {{ denominator }},
                                                                  average_proportion = average,
                                                                  side = "low",
                                                                  multiplier = multiplier) ~ "Low (0.001)",
      multiplier * {{ x }} / {{ denominator }} < sigma_adjustment(p = 0.975,
                                                                  population = {{ denominator }},
                                                                  average_proportion = average,
                                                                  side = "low",
                                                                  multiplier = multiplier) ~ "Low (0.025)",
      multiplier * {{ x }} / {{ denominator }} > sigma_adjustment(p = 0.999,
                                                                  population = {{ denominator }},
                                                                  average_proportion = average,
                                                                  side = "high",
                                                                  multiplier = multiplier) ~ "High (0.001)",
      multiplier * {{ x }} / {{ denominator }} > sigma_adjustment(p = 0.975,
                                                                  population = {{ denominator }},
                                                                  average_proportion = average,
                                                                  side = "high",
                                                                  multiplier = multiplier) ~ "High (0.025)",
      TRUE ~ "Not significant"
    ),
            significance = factor(significance))
  return(significance)
}

