# -------------------------------------------------------------------------------------------------
#' phe_quantiles
#'
#' Assigns small areas to quantiles based on numeric data rankings.
#'
#' @param data a data.frame containing the base and higher geography data and quantitive data for assigning quantiles,
#'             pre-grouped if quantiles required for breakdowns other than the defined higher geographies;
#'             unquoted string; no default
#' @param values field name from data containing the numeric values to rank data by and assign quantiles from;
#'          unquoted string; no default
#' @param basegeog field name from data containing the small area geographies to be assigned to quantiles;
#'          unquoted string; no default
#' @param highergeog field name from data containing the higher geographies to assign separate quantile categories within;
#'          unquoted string; no default
#' @param nquantiles the number of quantiles to assign per higher geography; numeric; default=10L
#' @param invert whether the quantiles should be directly (FALSE) or inversely (TRUE) related to the numerical value order;
#'               unquoted string referencing logical values as either a numeric vector or field name from data
#'               depending on value of inverttype; default = TRUE (highest values assigned to quantile 1)
#' @param inverttype whether the invert argument has been specified as a vector or a field name from data;
#'                   quoted string "field" or "vector"; default = "vector"
#'
#' @inheritParams phe_dsr
#'
#' @return When type = "full", returns the original data.frame with quantile field appended and
#'         named according to nquantiles
#'
#' @importFrom rlang sym quo_name
#'
#' @section Notes: See [PHE Technical Guide](https://fingertips.phe.org.uk/profile/guidance) for methodology.
#'          In particular, note that this function strictly applies the algorithm defined but some manual
#'          review and potentially adjutustment is advised in some cases where multiple small areas with equal rank
#'          fall across a natural quantile boundary.
#'
#' @examples
#'
#'
#'
#'
#'
#' @import dplyr
#'
#' @export
#'
#' @family PHEindicatormethods package functions
# -------------------------------------------------------------------------------------------------


# temp for function testing

# read in test data
library(fingertipsR)
library(dplyr)
fd <- fingertips_data(IndicatorID = c(90366,40501, 40502), AreaTypeID = 102, rank = TRUE) %>%
  group_by(IndicatorID, Sex) %>%
  filter(TimeperiodSortable == max(TimeperiodSortable) & AreaType == "County & UA") %>%
  select(IndicatorID, IndicatorName, ParentCode, ParentName, AreaCode, AreaName, Rank, Polarity, Sex, Value, AreaValuesCount) %>%
  mutate(Polarity_i = if_else(Polarity == "RAG - High is good",FALSE,TRUE))

  data <- fd
  values <- "Value"
  basegeog <- "AreaCode"
  highergeog <- "ParentCode"
  nquantiles <- 10L
  invert <- "Polarity_i"
  inverttype <- "field"


check <- phe_quantiles(data=fd, values=Value, basegeog = AreaCode,
                       highergeog = ParentCode, invert = Polarity_i, inverttype = "field")


# create phe_proportion function using Wilson's method
phe_quantiles <- function(data, values, basegeog, highergeog,
                          nquantiles=10L, invert=TRUE, inverttype = "vector") {

  # check required arguments present
  if (missing(data)|missing(values)|missing(basegeog)|missing(highergeog)) {
    stop("function phe_quantiles requires at least 4 arguments: data, values, smallarea, highergeog")
  }

  # check same number of rows per group
  if (n_distinct(select(ungroup(count(data)),n)) != 1) {
    stop("data must contain the same number of rows for each group")
  }

  # check invert is valid and append to data
  if (!(inverttype %in% c("vector","field"))) {
    stop("valid values for inverttype are vector and field")
  } else if (inverttype == "vector") {
    if (pull(slice(select(ungroup(count(data)),n),1)) %% length(invert) != 0) {
      stop("invert length must be a factor of the number of rows in each group within data")
    }
    data <- mutate(data,invert_calc = invert)
  } else if (inverttype == "field") {
    invert_q <- enquo(invert)
    if(1==1) {
#    if (deparse(substitute(invert)) %in% colnames(data)) {
#     if (invert %in% colnames(data)) {
       data <- rename(data,invert_calc = !!invert_q)
    } else stop("invert argument is not a field name from data")
  }


  # apply quotes to field names
  values_q     <- enquo(values)
  smallarea_q  <- enquo(basegeog)
  highergeog_q <- enquo(highergeog)

  # error handling for valid data types and values
  if (!(is.numeric(pull(data, !!values_q)))) {
      stop("values argument must be a numeric field from data")
  ##check all invert values are TRUE or FALSE by groups and highergeogs
  } else if (all(select(ungroup(count(group_by(data,invert_calc,add=TRUE))),n)) != n_distinct(select(ungroup(count(data)),n))) {
    stop("invert values must take the same logical value for each data grouping set and highergeog")
  }

  # assign field name for quantiles
  qname <- qnames$qname[qnames$quantiles == nquantiles]

  # assign quantiles
  phe_quantiles <- data %>%
    group_by(!!highergeog_q, add=TRUE) %>%
#    add_tally() %>%
#    mutate(adj_value = if_else(!!polarity_q %in% c("RAG - Low is good","Low"),max(!!values_q,na.rm=TRUE)-!!values_q,!!values_q),
    add_count(is.na(Value)) %>%
    mutate(adj_value = if_else(invert_calc == TRUE,max(!!values_q,na.rm=TRUE)-!!values_q,!!values_q),
           rank = rank(adj_value, ties.method="min", na.last = "keep"),
           qname = floor((nquantiles+1)-ceiling(((n+1)-rank)/(n/nquantiles))))

  return(phe_quantiles)
}
