# -------------------------------------------------------------------------------------------------
#' phe_quantiles
#'
#' Assigns small areas to quantiles based on numeric data rankings.
#'
#' @param data a data.frame containing the geography data and quantitive data for assigning quantiles;
#'             unquoted string; no default
#' @param values field name from data containing the numeric values to rank data by and assign quantiles from;
#'          unquoted string; no default
#' @param basegeog field name from data containing the small area geographies to be assigned to quantiles;
#'          unquoted string; no default
#' @param highergeog field name from data containing the higher geographies to assign separate quantile categories within;
#'          unquoted string; no default
#' @param quantiles the number of quantiles to assign per higher geography; numeric; default=10L
#' @param dir the direction for ranking values, ascending or descending; quoted string; default="ascending"
#'
#' @inheritParams phe_dsr
#'
#' @return When type = "full", returns the original data.frame with the following appended:
#'         quantile, statistic and method
#'
#' @importFrom rlang sym quo_name
#'
#' @section Notes: See [PHE Technical Guide](https://fingertips.phe.org.uk/profile/guidance) for methodology.
#'
#' @examples
#'
#' @import dplyr
#'
#' @export
#'
#' @family PHEindicatormethods package functions
# -------------------------------------------------------------------------------------------------

# create phe_proportion function using Wilson's method
phe_quantiles <- function(data, values, smallarea, highergeog,
                          quantiles=10L, dir = "ascending", type="standard", confidence=0.95, percentage=FALSE) {




  df <- data %>%
    arrange(highergeog,desc(values)) %>%
    group_by(highergeog) %>%
    add_tally() %>%
    mutate(rank = (n+1) - rank(values,ties.method="max"),
           quantile = floor((quantiles+1)-ceiling(((n+1)-rank)/(n/quantiles))))


}
