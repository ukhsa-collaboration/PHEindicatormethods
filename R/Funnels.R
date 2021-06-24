

sigma_adjustment <- function(p, population, average_proportion, side, multiplier) {
  # ((av * (t[j, "Population"] / qnorm(0.999)^2 + 1) +
  #     sigma_adjustment(t, 0.999, j) / 8) /
  #    (1 / qnorm(0.999)^2 + 1 / t[j, "Population"]) /
  #    t[j, "Population"]) * multiplier
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
#' Calculate confidence intervals/control limits and levels of significance
#'
#' Calculates control limits adopting consistent method as per the PHE Fingertips Technical Guidance : https://fingertips.phe.org.uk/profile/guidance
#'
#' @param data a data.frame containing the data to calculate control limits for;
#'            unquoted string; no default
#'
#' @param x field name from data containing the observed numbers of cases in the sample meeting the
#'          required condition (the numerator for the CLs); unquoted string; no default
#' @param denominator field name from data containing the population(s) in the sample
#'               (the denominator for the CLs); unquoted string; no default
#' @param multiplier the multiplier used to express the final values (eg 100 = percentage); numeric; default 100
#' @param statistic string; type of statistic to inform funnel calculations. Currently only accepts "proportion"
#' @return returns the original data.frame with the following appended:
#'         lower 0.025 limit, upper 0.025 limit, lower 0.001 limit, upper 0.001 limit and
#'         baseline average
#'
#' @import dplyr
#' @importFrom rlang sym quo_name := .data
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

phe_funnels <- function(data, x, denominator,
                        multiplier = 100, statistic = "proportion") {

  # check required arguments present
  if (missing(data) | missing(x) | missing(denominator)) {
    stop("function phe_funnels requires at least 3 arguments: data, x, n")
  }


  # calculate the initialisation variables - record level data

  prop_calc <- data %>%
    mutate(prop = 100 * {{ x }} / {{ denominator }}) %>%
    pull()

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
  signif.ceiling <- function(x, percentage_up = 1.05){
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

    # First create a vector with the numbers 1 to 100 (as the plot will have 100 data points)
  # Create a blank matrix which will be populated with results obtained above

  t <- matrix(
    ncol = 6,
    nrow = 100
  )
  colnames(t) <- c(
    "Population",
    "Lower2s0025limit", "Upper2s0025limit",
    "Lower3s0001limit", "Upper3s0001limit",
    "Baseline"
  )

  t[1, "Population"] <- max(1, axis_minimum)

  t[1, "Lower2s0025limit"] <- max(0,
                                  sigma_adjustment(0.975, t[1, "Population"], av, "low", multiplier))
  t[1, "Upper2s0025limit"] <- min(100,
                                  sigma_adjustment(0.975, t[1, "Population"], av, "high", multiplier))
  t[1, "Lower3s0001limit"] <- max(0,
                                  sigma_adjustment(0.999, t[1, "Population"], av, "low", multiplier))
  t[1, "Upper3s0001limit"] <- min(100,
                                  sigma_adjustment(0.999, t[1, "Population"], av, "high", multiplier))
  t[1, "Baseline"] <- av * multiplier

  for (j in 2:100) {

    # t[j, "Row.number"] <- t[j - 1] + 1
    t[j, "Population"] <- max(round((axis_maximum / t[j - 1, "Population"])^(1 / (101 - j)) *
                                      t[j - 1, "Population"]),
                              t[j - 1, "Population"] + 1)
    t[j, "Lower2s0025limit"] <- max(0,
                                    sigma_adjustment(0.975, t[j, "Population"], av, "low", multiplier))
    t[j, "Upper2s0025limit"] <- min(100,
                                    sigma_adjustment(0.975, t[j, "Population"], av, "high", multiplier))
    t[j, "Lower3s0001limit"] <- max(0,
                                    sigma_adjustment(0.999, t[j, "Population"], av, "low", multiplier))
    t[j, "Upper3s0001limit"] <- min(100,
                                    sigma_adjustment(0.999, t[j, "Population"], av, "high", multiplier))
    t[j, "Baseline"] <- av * multiplier
  }

  t <- as.data.frame(t) %>%
    select(
      .data$Population,
      .data$Lower2s0025limit, .data$Upper2s0025limit,
      .data$Lower3s0001limit,
      .data$Upper3s0001limit, .data$Baseline
    )
  return(t)
}


# -------------------------------------------------------------------------------------------------
#' Calculate confidence intervals/control limits and levels of significance around a funnel plot for proportions
#'
#' Calculates control limits adopting consistent method as per the PHE Fingertips Technical Guidance : https://fingertips.phe.org.uk/profile/guidance
#'
#' @param data a data.frame containing the data to calculate control limits for,
#' @param x field name from data containing the observed numbers of cases in the sample meeting the required condition
#'          (the numerator for the CLs); unquoted string; no default
#' @param denominator field name from data containing the population(s) in the sample (the denominator for the CLs);
#'          unquoted string; no default
#' @param multiplier the multiplier used to express the final values (eg 100 = percentage); numeric; default 100
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
# -------------------------------------------------------------------------------------------------

# First function - to append the level of significance

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
