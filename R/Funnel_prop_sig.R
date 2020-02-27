# -------------------------------------------------------------------------------------------------
#' Calculate confidence intervals/control limits and levels of significance around a funnel plot for proportions
#'
#' Calculates control limits adopting consistent method as per the PHE Fingertips Technical Guidance : https://fingertips.phe.org.uk/profile/guidance
#'
#' @param data a data.frame containing the data to calculate control limits for,
#'
#' @param x field name from data containing the observed numbers of cases in the sample meeting the required condition
#'          (the numerator for the CLs); unquoted string; no default
#' @param denom field name from data containing the population(s) in the sample (the denominator for the CLs);
#'          unquoted string; no default
#' @param multiplier the multiplier used to express the final values (eg 100 = percentage); numeric; default 1
#'
#' @inheritParams phe_dsr
#'
#' @return returns the original data.frame with the significance level appended
#'
#'
#' @import dplyr
#' @importFrom rlang sym quo_name :=
#'
#'
#' @examples
#' library(dplyr)
#' df<-data.frame(Area= c("A","B","C","D"),
#'                numerator = c(10232,12321,15123,13213),
#'                denominator = c(15232,16123,17932,18475))
#' df%>%
#'     phe_fun_prop_sig(numerator, denominator)
#'
#'
#' @export
#'
#'
#' @family PHEindicatormethods package functions
# -------------------------------------------------------------------------------------------------

# First function - to append the level of significance

phe_fun_prop_sig <- function(data, x, denom, multiplier=100) {

  # check required arguments present
  if (missing(data)|missing(x)|missing(denom)) {
    stop("function phe_proportion requires at least 3 arguments: data, x, n")
  }

  # validate arguments
  if (any(pull(data, {{ x }}) < 0, na.rm=TRUE)) {
    stop("numerators must be greater than or equal to zero")
  } else if (any(pull(data, {{ denom }}) <= 0, na.rm=TRUE)) {
    stop("denominators must be greater than zero")
  } else if (any(pull(data, {{ x }}) > pull(data, {{ denom }}), na.rm=TRUE)) {
    stop("numerators must be less than or equal to denominator for a proportion statistic")
  }

  # calculate proportion and CIs
  # initialise the variables
  # create summary variables with dplyr
  tot_x<-data%>%
    summarise(tot_x = sum({{ x }}))

  tot_n<-data%>%
    summarise(tot_n = sum({{ denom }}))

  # extract the values from the dataframes
  tot_x <- tot_x$tot_x
  tot_n <- tot_n$tot_n

  # calculate the average proportion
  tot_av<-tot_x/tot_n*multiplier


  # write the function
  dt_sig<-data%>%
    mutate(low0_001 = pmax(0,((tot_av/multiplier*({{ denom }}/qnorm(0.999)^2+1)-
                                sqrt((-8*tot_av/multiplier*({{ denom }}/qnorm(0.999)^2+1))^2-
                                       64*(1/qnorm(0.999)^2 +1/{{ denom }})*tot_av/multiplier*
                                       ({{ denom }}*(tot_av/multiplier*({{ denom }}/qnorm(0.999)^2+2)-1)+
                                          qnorm(0.999)^2*(tot_av/multiplier-1)))/
                                8)/(1/qnorm(0.999)^2 +1/{{ denom }})/{{ denom }})*multiplier),
           low0_025 = pmax(0,((tot_av/multiplier*({{ denom }}/qnorm(0.975)^2+1)-
                                sqrt((-8*tot_av/multiplier*({{ denom }}/qnorm(0.975)^2+1))^2-
                                       64*(1/qnorm(0.975)^2 +1/{{ denom }})*tot_av/multiplier*
                                       ({{ denom }}*(tot_av/multiplier*({{ denom }}/qnorm(0.975)^2+2)-1)+
                                          qnorm(0.975)^2*(tot_av/multiplier-1)))/
                                8)/(1/qnorm(0.975)^2 +1/{{ denom }})/{{ denom }})*multiplier),
           high0_001 = pmin(multiplier,((tot_av/multiplier*({{ denom }}/qnorm(0.999)^2+1)+
                                          sqrt((-8*tot_av/multiplier*({{ denom }}/qnorm(0.999)^2+1))^2-
                                                 64*(1/qnorm(0.999)^2 +1/{{ denom }})*tot_av/multiplier*
                                                 ({{ denom }}*(tot_av/multiplier*({{ denom }}/qnorm(0.999)^2+2)-1)+
                                                    qnorm(0.999)^2*(tot_av/multiplier-1)))/
                                          8)/(1/qnorm(0.999)^2 +1/{{ denom }})/{{ denom }})*multiplier),
           high0_025 = pmin(multiplier,((tot_av/multiplier*({{ denom }}/qnorm(0.975)^2+1)+
                                          sqrt((-8*tot_av/multiplier*({{ denom }}/qnorm(0.975)^2+1))^2-
                                                 64*(1/qnorm(0.975)^2 +1/{{ denom }})*tot_av/multiplier*
                                                 ({{ denom }}*(tot_av/multiplier*({{ denom }}/qnorm(0.975)^2+2)-1)+
                                                    qnorm(0.975)^2*(tot_av/multiplier-1)))/
                                          8)/(1/qnorm(0.975)^2 +1/{{ denom }})/{{ denom }})*multiplier)) %>%
    mutate(significance = case_when(
      {{ x }}/{{ denom }}*multiplier<low0_001  ~ "Low (0.001)",

      {{ x }}/{{ denom }}*multiplier<low0_025  ~ "Low (0.025)",

      {{ x }}/{{ denom }}*multiplier>high0_001 ~ "High (0.001)",

      {{ x }}/{{ denom }}*multiplier>high0_025 ~ "High (0.025)",  TRUE ~ "")
    ) %>%
    select(-low0_001, -low0_025, -high0_001, -high0_025)

}

