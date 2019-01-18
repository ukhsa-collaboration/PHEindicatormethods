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
#' @param highergeog field name from data containing the higher geographies to assign separate quantile categories within if required;
#'          unquoted string; default = NULL
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


# create phe_quantile function using PHE method
phe_quantile <- function(data, values, basegeog, highergeog = NULL,
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
  if(!missing(highergeog)) {
    highergeog_q = enquo(highergeog)
  }

  # error handling for valid data types and values
  if (!(is.numeric(pull(data, !!values_q)))) {
      stop("values argument must be a numeric field from data")

  #check all invert values are identical within groups and highergeogs   #CHECK THIS WITH CHANGES TO ALLOW NO HIGHERGEOG TO BE SPECIFIED
  } else if (nrow(count(data,invert_calc)) != nrow(count(data))) {
    stop("invert field values must take the same logical value for each data grouping set and highergeog")
  }

  # assign quantiles
  if(!missing(highergeog)) {

      phe_quantile <- data %>%
      group_by(!!highergeog_q, add=TRUE) %>%
      add_count(naflag = is.na(Value)) %>%
      mutate(adj_value = if_else(invert_calc == TRUE,max(!!values_q,na.rm=TRUE)-!!values_q,!!values_q),
             rank = rank(adj_value, ties.method="min", na.last = "keep"),
             quantile = floor((nquantiles+1)-ceiling(((n+1)-rank)/(n/nquantiles))),
             quantile = if_else(quantile == 0,1,quantile)) %>%
      select(-naflag,-n,-adj_value, -rank)

  } else {

      phe_quantile <- data %>%
      add_count(naflag = is.na(Value)) %>%
      mutate(adj_value = if_else(invert_calc == TRUE,max(!!values_q,na.rm=TRUE)-!!values_q,!!values_q),
           rank = rank(adj_value, ties.method="min", na.last = "keep"),
           quantile = floor((nquantiles+1)-ceiling(((n+1)-rank)/(n/nquantiles))),
           quantile = if_else(quantile == 0,1,quantile)) %>%
        select(-naflag,-n,-adj_value, -rank)
  }

  #rename quantile column in output
  if (nquantiles %in% qnames$quantiles) {
    colname <- qnames$qname[qnames$quantiles == nquantiles]
    colnames(phe_quantile)[colnames(phe_quantile )=="quantile"] <- colname
  }

  # if invert provided as logical then delete invert_calc column created for calculation
  if (inverttype == "logical") {
    phe_quantile <- phe_quantile %>% select(-invert_calc)
  }


  return(phe_quantile)
}
