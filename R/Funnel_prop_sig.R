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
#'
#' @return returns the original data.frame with the following appended:
#'         lower 0.025 limit, upper 0.025 limit, lower 0.001 limit, upper 0.001 limit and
#'         baseline average
#'
#' @import dplyr
#' @importFrom rlang sym quo_name :=
#'
#' @examples
#' @export
#'
#' @family PHEindicatormethods package functions
# -------------------------------------------------------------------------------------------------

# Generate a dataframe of the confidence limits for plotting
# NB -this does not alter the original dataset but generates a dataframe of 100 records for
# plotting the control limits

phe_fun_prop_plot<-function(data, x, denominator){

  # check required arguments present
  if (missing(data) | missing(x) | missing(denominator)) {
    stop("function phe_proportion requires at least 3 arguments: data, x, n")
  }


  #calculate the initialisation variables - record level data

  prop <- data %>%
    mutate(prop = {{ x }} / {{ denominator }})

  prop <- prop$prop
  prop_calc <- prop * 100

  #aggregated data
  summaries <- data %>%
    summarise(av = sum({{ x }}) / sum({{ denominator }}),
              mind = min({{ denominator }}),
              maxd = max({{ denominator }}))
  av <- summaries$av
  mind <- summaries$mind
  maxd <- summaries$maxd

  if (maxd > 2 * mind) {
    actmind <- 0
  } else {
    actmind <- floor(mind * 0.95)
  }

  actmaxd <- ceiling((maxd * 1.05) /
                       10 ^ (nchar(floor(maxd * 1.05 / 10) * 10) - 2)) * 10 ^ (nchar(floor(maxd * 1.05 / 10) * 10) - 2)
  minp <- min(prop_calc)
  maxp <- max(prop_calc)

  if (maxp > 2 * minp) {
    actminp <- 0
  } else {
    actminp <- floor(minp * 0.95)
  }

  # First create a vector with the numbers 1 to 100 (as the plot will have 100 data points)

  # Create a blank matrix which will be populated with results obtained above

  t <- matrix(ncol = 7,
              nrow = 100)
  colnames(t) <- c("Row.number", "Population",
                   "Lower2s0025limit", "Upper2s0025limit",
                   "Lower3s0001limit", "Upper3s0001limit",
                   "Baseline")
  t[1,1] = 1
  t[1,2] = 1
  t[1,3] = max(0, ((av * (t[1, 2] / qnorm(0.975) ^ 2 + 1) -
                      sqrt((- 8 * av * (t[1, 2] / qnorm(0.975) ^ 2 + 1)) ^ 2 - 64 *
                             (1 / qnorm(0.975) ^ 2 + 1 / t[1, 2]) * av * (t[1, 2] *
                                                                            (av * (t[1, 2] / qnorm(0.975) ^ 2 + 2) - 1) + qnorm(0.975) ^ 2 *
                                                                            (av - 1))) / 8) / (1 / qnorm(0.975) ^ 2 + 1 / t[1, 2]) / t[1, 2]) * 100)
  t[1,4] = min(100,((av*(t[1,2]/qnorm(0.975)^2+1)+sqrt((-8*av*(t[1,2]/qnorm(0.975)^2+1))^2-64*(1/qnorm(0.975)^2+1/t[1,2])*av*(t[1,2]*(av*(t[1,2]/qnorm(0.975)^2+2)-1)+qnorm(0.975)^2*
                                                                                                                               (av-1)))/8)/(1/qnorm(0.975)^2+1/t[1,2])/t[1,2])*100)
  t[1,5] = max(0,((av*(t[1,2]/qnorm(0.999)^2+1)-sqrt((-8*av*(t[1,2]/qnorm(0.999)^2+1))^2-64*(1/qnorm(0.999)^2+1/t[1,2])*av*(t[1,2]*(av*(t[1,2]/qnorm(0.999)^2+2)-1)+qnorm(0.999)^2*
                                                                                                                             (av-1)))/8)/(1/qnorm(0.999)^2+1/t[1,2])/t[1,2])*100)
  t[1,6] = min(100,((av*(t[1,2]/qnorm(0.999)^2+1)+sqrt((-8*av*(t[1,2]/qnorm(0.999)^2+1))^2-64*(1/qnorm(0.999)^2+1/t[1,2])*av*(t[1,2]*(av*(t[1,2]/qnorm(0.999)^2+2)-1)+qnorm(0.999)^2*
                                                                                                                               (av-1)))/8)/(1/qnorm(0.999)^2+1/t[1,2])/t[1,2])*100)
  t[1,7] = av * 100

  for(j in 2:100) {
    t[j,1]=t[j-1]+1
    t[j,2]=max(round((actmaxd/t[j-1,2])^(1/(101-t[j,1]))*t[j-1,2]),t[j-1,2]+1)
    t[j,3]= max(0,((av*(t[j,2]/qnorm(0.975)^2+1)-sqrt((-8*av*(t[j,2]/qnorm(0.975)^2+1))^2-64*(1/qnorm(0.975)^2+1/t[j,2])*av*(t[j,2]*(av*(t[j,2]/qnorm(0.975)^2+2)-1)+qnorm(0.975)^2*
                                                                                                                               (av-1)))/8)/(1/qnorm(0.975)^2+1/t[j,2])/t[j,2])*100)
    t[j,4]= min(100,((av*(t[j,2]/qnorm(0.975)^2+1)+sqrt((-8*av*(t[j,2]/qnorm(0.975)^2+1))^2-64*(1/qnorm(0.975)^2+1/t[j,2])*av*(t[j,2]*(av*(t[j,2]/qnorm(0.975)^2+2)-1)+qnorm(0.975)^2*
                                                                                                                                 (av-1)))/8)/(1/qnorm(0.975)^2+1/t[j,2])/t[j,2])*100)
    t[j,5]= max(0,((av*(t[j,2]/qnorm(0.999)^2+1)-sqrt((-8*av*(t[j,2]/qnorm(0.999)^2+1))^2-64*(1/qnorm(0.999)^2+1/t[j,2])*av*(t[j,2]*(av*(t[j,2]/qnorm(0.999)^2+2)-1)+qnorm(0.999)^2*
                                                                                                                               (av-1)))/8)/(1/qnorm(0.999)^2+1/t[j,2])/t[j,2])*100)
    t[j,6]= min(100,((av*(t[j,2]/qnorm(0.999)^2+1)+sqrt((-8*av*(t[j,2]/qnorm(0.999)^2+1))^2-64*(1/qnorm(0.999)^2+1/t[j,2])*av*(t[j,2]*(av*(t[j,2]/qnorm(0.999)^2+2)-1)+qnorm(0.999)^2*                                                                                                                                 (av-1)))/8)/(1/qnorm(0.999)^2+1/t[j,2])/t[j,2])*100)
    t[j,7]= av*100
  }
  t<-as.data.frame(t)
  t<-t%>%
    select("Population","Lower2s0025limit","Upper2s0025limit","Lower3s0001limit",
           "Upper3s0001limit","Baseline","Row.number")
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
#' @param multiplier the multiplier used to express the final values (eg 100 = percentage); numeric; default 1
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
#' df <- data.frame(Area= c("A", "B", "C", "D"),
#'                  numerator = c(10232, 12321, 15123, 13213),
#'                  denominator = c(15232, 16123, 17932, 18475))
#' df %>%
#'     phe_fun_prop_sig(numerator, denominator)
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
    mutate(low0_001 = pmax(0, ((tot_av / multiplier * ({{ denominator }} / qnorm(0.999) ^ 2 + 1) -
                                sqrt((- 8 * tot_av / multiplier * ({{ denominator }} / qnorm(0.999) ^ 2 + 1)) ^ 2 -
                                       64 * (1 / qnorm(0.999) ^ 2 + 1 / {{ denominator }}) * tot_av / multiplier *
                                       ({{ denominator }} * (tot_av / multiplier * ({{ denominator }} / qnorm(0.999) ^ 2 + 2) - 1)+
                                          qnorm(0.999) ^ 2 * (tot_av / multiplier - 1))) /
                                8) / (1 / qnorm(0.999) ^ 2 + 1 / {{ denominator }}) / {{ denominator }}) * multiplier),
           low0_025 = pmax(0, ((tot_av / multiplier * ({{ denominator }} / qnorm(0.975) ^ 2 + 1) -
                                sqrt((-8 * tot_av / multiplier * ({{ denominator }} / qnorm(0.975) ^ 2 + 1)) ^ 2 -
                                       64 * (1 / qnorm(0.975) ^ 2 + 1 / {{ denominator }}) * tot_av / multiplier *
                                       ({{ denominator }} * (tot_av / multiplier * ({{ denominator }} / qnorm(0.975) ^ 2 + 2) - 1) +
                                          qnorm(0.975) ^ 2 *(tot_av / multiplier - 1))) /
                                8) / (1 / qnorm(0.975) ^ 2 + 1 / {{ denominator }}) / {{ denominator }}) * multiplier),
           high0_001 = pmin(multiplier, ((tot_av / multiplier * ({{ denominator }} / qnorm(0.999) ^ 2 + 1) +
                                          sqrt((- 8 * tot_av / multiplier * ({{ denominator }} / qnorm(0.999) ^ 2 + 1)) ^ 2 -
                                                 64 * (1 / qnorm(0.999) ^ 2 + 1 / {{ denominator }}) * tot_av / multiplier *
                                                 ({{ denominator }} * (tot_av / multiplier * ({{ denominator }} / qnorm(0.999) ^ 2 + 2) - 1) +
                                                    qnorm(0.999) ^ 2 * (tot_av / multiplier - 1))) /
                                          8) / (1 / qnorm(0.999) ^ 2 + 1 / {{ denominator }}) / {{ denominator }}) * multiplier),
           high0_025 = pmin(multiplier, ((tot_av / multiplier * ({{ denominator }} / qnorm(0.975) ^ 2 + 1) +
                                          sqrt((- 8 * tot_av / multiplier * ({{ denominator }} / qnorm(0.975) ^ 2 + 1)) ^ 2 -
                                                 64 * (1 / qnorm(0.975) ^ 2 + 1 / {{ denominator }}) * tot_av / multiplier *
                                                 ({{ denominator }} * (tot_av / multiplier * ({{ denominator }} / qnorm(0.975) ^ 2 + 2) - 1) +
                                                    qnorm(0.975) ^ 2 * (tot_av / multiplier - 1))) /
                                          8) / (1 / qnorm(0.975) ^ 2 + 1 / {{ denominator }}) / {{ denominator }}) * multiplier)) %>%
    mutate(significance = case_when(
      {{ x }} * {{ denominator }} * multiplier * low0_001  ~ "Low (0.001)",
      {{ x }} * {{ denominator }} * multiplier * low0_025  ~ "Low (0.025)",
      {{ x }} * {{ denominator }} * multiplier * high0_001 ~ "High (0.001)",
      {{ x }} * {{ denominator }} * multiplier * high0_025 ~ "High (0.025)",
      TRUE ~ "")
    ) %>%
    select(!c(low0_001, low0_025, high0_001, high0_025))

}

