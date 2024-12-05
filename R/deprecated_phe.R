# SCRIPT CONTAINS ALL SOFT-DEPRECATED PHE FUNCTIONS TO BE REMOVED NOT BEFORE SEPTEMBER 2025

# -------------------------------------------------------------------------------------------------
#' Calculate Means using phe_mean
#'
#' Calculates means with confidence limits using Student's t-distribution method.
#'
#' @param data a data.frame containing the data to calculate means for, pre-grouped if multiple means required; unquoted string; no default
#' @param x field name from data containing the values to calculate the means for; unquoted string; no default
#'
#' @inheritParams calculate_dsr
#'
#' @import dplyr
#' @importFrom rlang sym quo_name
#' @importFrom stats qt sd
#' @export
#'
#' @return When type = "full", returns a data.frame of value_sum, value_count, stdev, value, lowercl, uppercl, confidence, statistic and method
#'         for each grouping set
#'
#' @examples
#' library(dplyr)
#' df <- data.frame(values = c(30,40,50,60))
#'
#' ## default execution
#' phe_mean(df, values)
#' ->
#' calculate_mean(df, values)
#'
#' ## calculate 95% and 99.8% CIs in single execution
#' phe_mean(df, values, confidence = c(0.95, 0.998))
#' ->
#' calculaue_mean(df, values, confidence = c(0.95, 0.998))
#'
#' ## calculate multiple means in a single execution
#' df2 <- data.frame(area = rep(c("Area1", "Area2"),each=3),
#'                   values = c(20,30,40,200,300,400)) %>%
#'                   group_by(area)
#'
#' phe_mean(df2,values)
#' ->
#' calculate_mean(df2,values)
#'
#' phe_mean(df2,values,type="standard", confidence=0.998)
#' ->
#' calculate_mean(df2,values,type="standard", confidence=0.998)
#'
#'
#' @family PHEindicatormethods package functions
# -------------------------------------------------------------------------------------------------

# create phe_mean function as soft-deprecated wrapper for calculate_mean
phe_mean <- function(data, x, type = "full", confidence = 0.95) {

  warning("lifecycle soft-deprecation message")

  mean <- calculate_mean(data, x, type, confidence)

  return(mean)
}
