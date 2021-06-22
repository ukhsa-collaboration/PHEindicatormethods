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
#' @return returns the original data.frame with the following appended:
#'         lower 0.025 limit, upper 0.025 limit, lower 0.001 limit, upper 0.001 limit and
#'         baseline average
#'
#' @import dplyr
#' @importFrom rlang sym quo_name :=
#'
#' @examples
#' @export
#' @author Matthew Francis, \email{Matthew.Francis@@phe.gov.uk}
#'
#' @family PHEindicatormethods package functions
# -------------------------------------------------------------------------------------------------

# Generate a dataframe of the confidence limits for plotting
# NB -this does not alter the original dataset but generates a dataframe of 100 records for
# plotting the control limits

phe_funnels <- function(data, x, denominator,
                        multiplier = 100) {

  # check required arguments present
  if (missing(data) | missing(x) | missing(denominator)) {
    stop("function phe_proportion requires at least 3 arguments: data, x, n")
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

  if (max_denominator > 2 * min_denominator) {
    axis_minimum <- 0
  } else {
    axis_minimum <- floor(min_denominator * 0.95)
  }

  axis_maximum <- ceiling((max_denominator * 1.05) /
    10^(nchar(floor(max_denominator * 1.05 / 10) * 10) - 2)) * 10^(nchar(floor(max_denominator * 1.05 / 10) * 10) - 2)
  min_percent <- min(prop_calc)
  max_percent <- max(prop_calc)

  if (max_percent > 2 * min_percent) {
    min_percent_axis <- 0
  } else {
    min_percent_axis <- floor(min_percent * 0.95)
  }

  # First create a vector with the numbers 1 to 100 (as the plot will have 100 data points)
  # Create a blank matrix which will be populated with results obtained above

  t <- matrix(
    ncol = 7,
    nrow = 100
  )
  colnames(t) <- c(
    "Row.number", "Population",
    "Lower2s0025limit", "Upper2s0025limit",
    "Lower3s0001limit", "Upper3s0001limit",
    "Baseline"
  )
  t[1, "Row.number"] <- 1
  t[1, "Population"] <- max(1, axis_minimum)



  sigma_adjustment <- function(table, p, population_row, average, side, multiplier) {
    # ((av * (t[j, "Population"] / qnorm(0.999)^2 + 1) +
    #     sigma_adjustment(t, 0.999, j) / 8) /
    #    (1 / qnorm(0.999)^2 + 1 / t[j, "Population"]) /
    #    t[j, "Population"]) * multiplier
    first_part <- average * (table[population_row, "Population"] /
                          qnorm(p)^2 + 1)

    adj <- sqrt((-8 * av * (table[population_row, "Population"] /
                              qnorm(p)^2 + 1))^2 - 64 *
                  (1 / qnorm(p)^2 + 1 / table[population_row, "Population"]) *
                  av * (table[population_row, "Population"] *
                          (av * (table[population_row, "Population"] /
                                   qnorm(p)^2 + 2) - 1) +
                          qnorm(p)^2 *
                          (av - 1)))

    last_part <- (1 / qnorm(p)^2 + 1 /
                    table[population_row, "Population"])

    if (side == "low") {
      adj_return <- (first_part - adj / 8) / last_part
    } else if (side == "high") {
      adj_return <- (first_part + adj / 8) / last_part
    }
    adj_return <- (adj_return /
      table[population_row, "Population"]) * multiplier
    return(adj_return)
  }



  t[1, "Lower2s0025limit"] <- max(0,
                                  sigma_adjustment(t, 0.975, 1, av, "low", multiplier))
  t[1, "Upper2s0025limit"] <- min(100,
                                  sigma_adjustment(t, 0.975, 1, av, "high", multiplier))
  t[1, "Lower3s0001limit"] <- max(0,
                                  sigma_adjustment(t, 0.999, 1, av, "low", multiplier))
  t[1, "Upper3s0001limit"] <- min(100,
                                  sigma_adjustment(t, 0.999, 1, av, "high", multiplier))
  t[1, "Baseline"] <- av * multiplier

  for (j in 2:100) {

    t[j, "Row.number"] <- t[j - 1] + 1
    t[j, "Population"] <- max(round((axis_maximum / t[j - 1, "Population"])^(1 / (101 - t[j, "Row.number"])) *
                                      t[j - 1, "Population"]),
                              t[j - 1, "Population"] + 1)
    t[j, "Lower2s0025limit"] <- max(0,
                                    sigma_adjustment(t, 0.975, j, av, "low", multiplier))
    t[j, "Upper2s0025limit"] <- min(100,
                                    sigma_adjustment(t, 0.975, j, av, "high", multiplier))
    t[j, "Lower3s0001limit"] <- max(0,
                                    sigma_adjustment(t, 0.999, j, av, "low", multiplier))
    t[j, "Upper3s0001limit"] <- min(100,
                                    sigma_adjustment(t, 0.999, j, av, "high", multiplier))
    t[j, "Baseline"] <- av * multiplier
  }

  t <- as.data.frame(t) %>%
    select(
      Population,
      Lower2s0025limit, Upper2s0025limit,
      Lower3s0001limit,
      Upper3s0001limit, Baseline,
      Row.number
    )
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
#'
#' @inheritParams phe_dsr
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
#'   phe_fun_prop_sig(numerator, denominator)
#' @export
#'
#' @family PHEindicatormethods package functions
# -------------------------------------------------------------------------------------------------

# First function - to append the level of significance

phe_funnel_significance <- function(data, x, denominator,
                                    multiplier = 100, statistic = "proportion") {

  # check required arguments present
  if (missing(data) | missing(x) | missing(denominator)) {
    stop("function phe_proportion requires at least 3 arguments: data, x, n")
  }

  # validate arguments
  if (any(pull(data, {{ x }}) < 0, na.rm = TRUE)) {
    stop("numerators must be greater than or equal to zero")
  } else if (any(pull(data, {{ denominator }}) <= 0, na.rm = TRUE)) {
    stop("denominators must be greater than zero")
  } else if (any(pull(data, {{ x }}) > pull(data, {{ denominator }}), na.rm = TRUE)) {
    stop("numerators must be less than or equal to denominator for a proportion statistic")
  }

  # calculate proportion and CIs
  # initialise the variables
  # create summary variables with dplyr
  tot_x <- data %>%
    summarise(tot_x = sum({{ x }}))

  tot_n <- data %>%
    summarise(tot_n = sum({{ denominator }}))

  # extract the values from the dataframes
  tot_x <- tot_x$tot_x
  tot_n <- tot_n$tot_n

  # calculate the average proportion
  tot_av <- tot_x / tot_n * multiplier


  # write the function
  dt_sig <- data %>%
    mutate(
      low0_001 = pmax(0, ((tot_av / multiplier * ({{ denominator }} / qnorm(0.999)^2 + 1) -
        sqrt((-8 * tot_av / multiplier * ({{ denominator }} / qnorm(0.999)^2 + 1))^2 -
          64 * (1 / qnorm(0.999)^2 + 1 / {{ denominator }}) * tot_av / multiplier *
            ({{ denominator }} * (tot_av / multiplier * ({{ denominator }} / qnorm(0.999)^2 + 2) - 1) +
              qnorm(0.999)^2 * (tot_av / multiplier - 1))) /
          8) / (1 / qnorm(0.999)^2 + 1 / {{ denominator }}) / {{ denominator }}) * multiplier),
      low0_025 = pmax(0, ((tot_av / multiplier * ({{ denominator }} / qnorm(0.975)^2 + 1) -
        sqrt((-8 * tot_av / multiplier * ({{ denominator }} / qnorm(0.975)^2 + 1))^2 -
          64 * (1 / qnorm(0.975)^2 + 1 / {{ denominator }}) * tot_av / multiplier *
            ({{ denominator }} * (tot_av / multiplier * ({{ denominator }} / qnorm(0.975)^2 + 2) - 1) +
              qnorm(0.975)^2 * (tot_av / multiplier - 1))) /
          8) / (1 / qnorm(0.975)^2 + 1 / {{ denominator }}) / {{ denominator }}) * multiplier),
      high0_001 = pmin(multiplier, ((tot_av / multiplier * ({{ denominator }} / qnorm(0.999)^2 + 1) +
        sqrt((-8 * tot_av / multiplier * ({{ denominator }} / qnorm(0.999)^2 + 1))^2 -
          64 * (1 / qnorm(0.999)^2 + 1 / {{ denominator }}) * tot_av / multiplier *
            ({{ denominator }} * (tot_av / multiplier * ({{ denominator }} / qnorm(0.999)^2 + 2) - 1) +
              qnorm(0.999)^2 * (tot_av / multiplier - 1))) /
          8) / (1 / qnorm(0.999)^2 + 1 / {{ denominator }}) / {{ denominator }}) * multiplier),
      high0_025 = pmin(multiplier, ((tot_av / multiplier * ({{ denominator }} / qnorm(0.975)^2 + 1) +
        sqrt((-8 * tot_av / multiplier * ({{ denominator }} / qnorm(0.975)^2 + 1))^2 -
          64 * (1 / qnorm(0.975)^2 + 1 / {{ denominator }}) * tot_av / multiplier *
            ({{ denominator }} * (tot_av / multiplier * ({{ denominator }} / qnorm(0.975)^2 + 2) - 1) +
              qnorm(0.975)^2 * (tot_av / multiplier - 1))) /
          8) / (1 / qnorm(0.975)^2 + 1 / {{ denominator }}) / {{ denominator }}) * multiplier)
    ) %>%
    mutate(significance = case_when(
      {{ x }} * {{ denominator }} * multiplier * low0_001 ~ "Low (0.001)",
      {{ x }} * {{ denominator }} * multiplier * low0_025 ~ "Low (0.025)",
      {{ x }} * {{ denominator }} * multiplier * high0_001 ~ "High (0.001)",
      {{ x }} * {{ denominator }} * multiplier * high0_025 ~ "High (0.025)",
      TRUE ~ ""
    )) %>%
    select(!c(low0_001, low0_025, high0_001, high0_025))
}
