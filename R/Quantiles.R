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
#'               unquoted string referencing logical values as either a single logical value or field name from data
#'               depending on value of inverttype; default = TRUE (ie highest values assigned to quantile 1)
#' @param inverttype whether the invert argument has been specified as a single logical value or a field name from data;
#'                   quoted string "field" or "logical"; default = "logical"
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
# library(fingertipsR)
# library(dplyr)
# fd <- fingertips_data(IndicatorID = c(90366,40501, 40502), AreaTypeID = 102, rank = TRUE) %>%
#   group_by(IndicatorID, Sex) %>%
#   filter(TimeperiodSortable == max(TimeperiodSortable) & AreaType == "County & UA") %>%
#   select(IndicatorID, IndicatorName, ParentCode, ParentName, AreaCode, AreaName, Rank, Polarity, Sex, Value, AreaValuesCount) %>%
#   mutate(Polarity_i = if_else(Polarity == "RAG - High is good",FALSE,TRUE),
#          Ref_ug = paste0(IndicatorID,Sex))
#
#   data <- test_quantiles_g
#   values <- "Value"
#   basegeog <- "AreaCode"
#   highergeog <- "ParentCode"
#   nquantiles <- 7L
#   invert <- test_quantiles_g$Polarity
#   inverttype <- "vector"
#
#
#
# check1 <- phe_quantiles(data=fd, values=Value, basegeog = AreaCode,
#                        highergeog = ParentCode, invert = Polarity_i, inverttype = "field")
#
# check2 <- phe_quantiles(data=fd, values=Value, basegeog = AreaCode,
#                         highergeog = Ref_ug, nquantiles = 7L, invert = Polarity_i, inverttype = "field")
#
# check3 <- phe_quantiles(data=fd, values=Value, basegeog = AreaCode,
#                         highergeog = Ref_ug, nquantiles = 7L, invert = FALSE, inverttype = "logical")

## NEED TO EDIT SO HIGHERGEOG NOT REAUIRED (EG FOR NATIONAL QUANTILES)


# create phe_quantile function using PHE method
phe_quantile <- function(data, values, basegeog, highergeog,
                          nquantiles=10L, invert=TRUE, inverttype = "logical") {

  # check required arguments present
  if (missing(data)|missing(values)|missing(basegeog)) {
    stop("function phe_quantile requires at least 3 arguments: data, values, basegeog")
  }

  # check invert is valid and append to data
  if (!(inverttype %in% c("logical","field"))) {
    stop("valid values for inverttype are logical and field")
  } else if (inverttype == "logical") {
    if (!(invert %in% c(TRUE,FALSE))) {
      stop("invert expressed as a logical must equal TRUE or FALSE")
    }
    data <- mutate(data,invert_calc = invert)
  } else if (inverttype == "field") {
    invert_q <- enquo(invert)
    data <- rename(data,invert_calc = !!invert_q)
  }


  # apply quotes to field names
  values_q     <- enquo(values)
  smallarea_q  <- enquo(basegeog)
  highergeog_q <- enquo(highergeog)

  # error handling for valid data types and values
  if (!(is.numeric(pull(data, !!values_q)))) {
      stop("values argument must be a numeric field from data")
  #check all invert values are identical within groups and highergeogs
  } else if (nrow(count(data,invert_calc)) != nrow(count(data))) {
    stop("invert field values must take the same logical value for each data grouping set and highergeog")
  }

  # assign quantiles
  phe_quantile <- data %>%
    group_by(!!highergeog_q, add=TRUE) %>%
    add_count(is.na(Value)) %>%
    mutate(adj_value = if_else(invert_calc == TRUE,max(!!values_q,na.rm=TRUE)-!!values_q,!!values_q),
           rank = rank(adj_value, ties.method="min", na.last = "keep"),
           quantile = floor((nquantiles+1)-ceiling(((n+1)-rank)/(n/nquantiles))),
           quantile = if_else(quantile == 0,1,quantile))

#rename quantile column in output - not working
# names(phe_quantiles$quantile) <- qnames$qname[qnames$quantiles == nquantiles]

  return(phe_quantile)
}
