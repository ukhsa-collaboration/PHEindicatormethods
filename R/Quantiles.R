# ------------------------------------------------------------------------------
#' Assign Quantiles using phe_quantile
#'
#' Assigns data to quantiles based on numeric data rankings.
#'
#' @param data a data frame containing the quantitative data to be assigned to
#'   quantiles. If pre-grouped, separate sets of quantiles will be assigned for
#'   each grouping set; unquoted string; no default
#' @param values field name from data containing the numeric values to rank data
#'   by and assign quantiles from; unquoted string; no default
#' @param nquantiles the number of quantiles to separate each grouping set into;
#'   numeric; default=10L
#' @param invert whether the quantiles should be directly (FALSE) or inversely
#'   (TRUE) related to the numerical value order; logical (to apply same value
#'   to all grouping sets) OR unquoted string referencing field name from data
#'   that stores logical values for each grouping set; default = TRUE (ie
#'   highest values assigned to quantile 1)
#' @param inverttype whether the invert argument has been specified as a logical
#'   value or a field name from data; quoted string "field" or "logical";
#'   default = "logical"
#' @param type defines whether to include metadata columns in output to
#'   reference the arguments passed; can be "standard" or "full"; quoted string;
#'   default = "full"
#'
#' @import dplyr
#' @importFrom rlang sym quo_name
#' @export
#'
#' @return When type = "full", returns the original data.frame with quantile
#'   (quantile value), nquantiles (number of quantiles requested), groupvars
#'   (grouping sets quantiles assigned within) and invert (indicating direction
#'   of quantile assignment) fields appended.
#'
#' @section Notes: See [OHID Technical Guide - Assigning Deprivation Categories](https://fingertips.phe.org.uk/profile/guidance/supporting-information/PH-methods)
#'   for methodology. In particular, note that this function strictly applies
#'   the algorithm defined but some manual review, and potentially adjustment,
#'   is advised in some cases where multiple small areas with equal rank fall
#'   across a natural quantile boundary.
#'
#' @examples
#' df <- data.frame(
#'   region    = as.character(rep(c("Region1","Region2","Region3","Region4"), each=250)),
#'   smallarea = as.character(paste0("Area",seq_along(1:1000))),
#'   vals      = as.numeric(sample(200, 1000, replace = TRUE)),
#'   stringsAsFactors = FALSE)
#'
#' # assign small areas to deciles across whole data frame
#' phe_quantile(df, vals)
#'
#' # assign small areas to deciles within regions by pre-grouping the data frame
#' library(dplyr)
#' df_grp <- df %>% group_by(region)
#' phe_quantile(df_grp, vals)
#'
#' # assign small areas to quintiles, where highest value = highest quantile
#' phe_quantile(df, vals, nquantiles = 5L, invert=FALSE)
#'
#' @family PHEindicatormethods package functions
# -------------------------------------------------------------------------------------------------

# create phe_quantile function using PHE method
phe_quantile <- function(data,
                         values,
                         nquantiles = 10L,
                         invert = TRUE,
                         inverttype = "logical",
                         type = "full") {


    # check required arguments present
    if (missing(data)|missing(values)) {
        stop(paste0("function phe_quantile requires at least 2 arguments: ",
                    "data and values"))
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
    }

    #check all invert values are identical within groups
    if (!n_groups(data) == nrow(unique(select(data,"invert_calc")))) {
        stop(paste0("invert field values must take the same logical value for ",
                    "each data grouping set"))
    }


    # assign quantiles - unless number of small areas within a  group is less
    # than number of quantiles requested
    phe_quantile <- data %>%
        # flag records with values, as NA values can't be assigned to quantiles
        mutate(hasvalue = if_else(is.na({{ values }}), 0, 1)) %>%
        # count records per group
        add_count(name = "num_small_areas", wt = .data$hasvalue) %>%
        mutate(rank = case_when(
                        .data$invert_calc == TRUE ~ rank(- {{ values }},
                                                         ties.method = "min",
                                                         na.last = "keep"),
                        TRUE ~ rank({{ values }},
                                    ties.method = "min",
                                    na.last = "keep")
                      ),
               quantile  = if_else(.data$num_small_areas < nquantiles,
                                     NA_real_,
                                     floor((nquantiles + 1) - ceiling(((.data$num_small_areas+1)-rank) /
                                                                      (.data$num_small_areas/nquantiles)
                                                                      )
                                           )
                                   ),
               quantile  = if_else(.data$quantile == 0, 1, .data$quantile)
    ) %>%
        select(!c("hasvalue", "num_small_areas", "rank")) %>%
        mutate(nquantiles= nquantiles,
               groupvars = paste0(group_vars(data),collapse = ", "),
               qinverted = if_else(.data$invert_calc == TRUE,
                                   "lowest quantile represents highest values",
                                   "lowest quantile represents lowest values"))

    # warn if any groups had too few snall areas with values to assign quantiles
    if (#nrow(filter(phe_quantile, !is.na({{ values}}) &
        #                           is.na(.data$quantile))) > 0 |
        nrow(filter(phe_quantile, all(is.na({{ values }})) |
                                  all(is.na(.data$quantile)))) > 0
        ) {
    warning(paste0("One or more groups had too few small areas with values to ",
                   "allow quantiles to be assigned"))
    }

    # remove columns if not required based on value of type argument
    if (type == "standard") {
        phe_quantile <- phe_quantile %>%
                            select(!c("nquantiles", "groupvars",
                                      "qinverted", "invert_calc"))
    } else {
        phe_quantile <- phe_quantile %>%
                            select(!c("invert_calc"))
    }


  return(phe_quantile)
}
