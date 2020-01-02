# -------------------------------------------------------------------------------------------------
#' Assign Quantiles using phe_quantile
#'
#' Assigns small areas to quantiles based on numeric data rankings.
#'
#' @param data a data.frame containing the base (and optionally higher) geography data and quantitative data for assigning quantiles,
#'             pre-grouped if quantiles required for breakdowns other than the defined higher geographies;
#'             unquoted string; no default
#' @param values field name from data containing the numeric values for each base geography area to rank data by and assign quantiles from;
#'          unquoted string; no default
# @param basegeog field name from data containing the small area geographies to be assigned to quantiles;
#          unquoted string; no default
#' @param highergeog field name from data containing the higher geographies to assign separate quantile categories within if required;
#'          unquoted string; default = NULL
#' @param nquantiles the number of quantiles to assign per higher geography; numeric; default=10L
#' @param invert whether the quantiles should be directly (FALSE) or inversely (TRUE) related to the numerical value order;
#'               unquoted string referencing logical values as either a single logical value or field name from data
#'               depending on value of inverttype; default = TRUE (ie highest values assigned to quantile 1)
#' @param inverttype whether the invert argument has been specified as a single logical value or a field name from data;
#'                   quoted string "field" or "logical"; default = "logical"
#' @param type defines whether to include metadata columns in output to reference the arguments passed;
#'             can be "standard" or "full"; quoted string; default = "full"
#'
#' @inheritParams phe_dsr
#'
#' @import dplyr
#' @importFrom rlang sym quo_name
#' @export
#'
#' @return When type = "full", returns the original data.frame with quantile (quantile value), nquantiles (number of quantiles requested)
#'         and invert (indicating direction of quantile assignment) fields appended.
#'
#'
#' @section Notes: See [PHE Technical Guide - Assigning Deprivation Quintiles](https://fingertips.phe.org.uk/profile/guidance) for methodology.
#'          In particular, note that this function strictly applies the algorithm defined but some manual
#'          review, and potentially adjustment, is advised in some cases where multiple small areas with equal rank
#'          fall across a natural quantile boundary.
#'
#' @examples
#'
#' df <- data.frame(region = as.character(rep(c("Region1","Region2","Region3","Region4"), each=250)),
#'                    smallarea = as.character(paste0("Area",seq_along(1:1000))),
#'                    vals = as.numeric(sample(200, 1000, replace = TRUE)),
#'                    stringsAsFactors=FALSE)
#'
#' # assign small areas to deciles within regions - method 1: assign grouping set within function
#' phe_quantile(df, vals, region)
#'
#' # assign small area to deciles within regions - method 2: pre-group input dataframe
#' library(dplyr)
#' df_grp <- df %>% group_by(region)
#' phe_quantile(df_grp, vals)
#'
#' # assign smallareas to decile across whole data frame (ignoring region)
#' phe_quantile(df, vals)
#'
#' # assign smallareas to quintiles within regions, where high val = lowest quantile
#' phe_quantile(df, vals, region, invert=FALSE)
#'
#' @family PHEindicatormethods package functions
# -------------------------------------------------------------------------------------------------


# create phe_quantile function using PHE method
phe_quantile_old <- function(data, values, highergeog = NULL, nquantiles=10L,
                         invert=TRUE, inverttype = "logical", type = "full") {


    # check required arguments present
    if (missing(data)|missing(values)) {
        stop("function phe_quantile requires at least 2 arguments: data and values")
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
      if (deparse(substitute(invert)) %in% colnames(data)) {
          data <- mutate(data,invert_calc = {{ invert }})
      } else stop("invert is not a field name from data")
    }


    # error handling for valid data types and values
    if (!(is.numeric(pull(data, {{ values }})))) {
        stop("values argument must be a numeric field from data")


    #check all invert values are identical within groups and highergeogs   #CHECK THIS WITH CHANGES TO ALLOW NO HIGHERGEOG TO BE SPECIFIED
    } else if (nrow(count(data,invert_calc)) != nrow(count(data))) {
        stop("invert field values must take the same logical value for each data grouping set and highergeog")
    }


    # check groups to reapply later
    orig_groups <- groups(data)

    # add highergeog to grouping set
    if(!missing(highergeog)) {
#        if (is.null(chk_groups)) {
#            data <- data %>%
#                    group_by({{ highergeog }})
#        } else {
            data <- data %>%
                    group_by({{ highergeog }}, add=TRUE)
#        }
    }

    # assign quantiles
    phe_quantile <- data %>%
        add_count(naflag = is.na({{ values }})) %>%
        mutate(adj_value = if_else(invert_calc == TRUE, max({{ values }}, na.rm=TRUE)-{{ values }},{{ values }}),
               rank      = rank(adj_value, ties.method="min", na.last = "keep"),
               quantile  = floor((nquantiles+1)-ceiling(((n+1)-rank)/(n/nquantiles))),
               quantile  = if_else(quantile == 0,1,quantile)) %>%
        select(-naflag,-n,-adj_value, -rank) %>%
        mutate(nquantiles        = nquantiles,
               highergeog_column = paste0(group_vars(data),collapse = ", "),
               qinverted         = if_else(invert_calc == TRUE,"lowest quantile represents highest values",
                                           "lowest quantile represents lowest values"))

    # regroup with original grouping sets
    # NOTE - dplyr::ungroup is being amended to allow selective ungrouping (Dec 2019)
    if (is.null(orig_groups)) {
      phe_quantile <- ungroup(phe_quantile)
    } else {
      phe_quantile <- phe_quantile %>%
        group_by(orig_groups)
    }

    # remove columns if not required based on value of type argument
    if (type == "standard") {
        phe_quantile <- phe_quantile %>%
                            select(-nquantiles, -highergeog_column, -qinverted, -invert_calc)
    } else {
        phe_quantile <- phe_quantile %>%
                            select(-invert_calc)
    }


  return(phe_quantile)
}
