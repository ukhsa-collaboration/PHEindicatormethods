# -------------------------------------------------------------------------------------------------
#' phe_quantiles
#'
#' Assigns small areas to quantiles based on numeric data rankings.
#'
#' @param data a data.frame containing the geography data and quantitive data for assigning quantiles,
#'             pre-grouped if quantiles required for breakdowns other than the defined higher geographies;
#'             unquoted string; no default
#' @param values field name from data containing the numeric values to rank data by and assign quantiles from;
#'          unquoted string; no default
#' @param basegeog field name from data containing the small area geographies to be assigned to quantiles;
#'          unquoted string; no default
#' @param highergeog field name from data containing the higher geographies to assign separate quantile categories within;
#'          unquoted string; no default
#' @param quantiles the number of quantiles to assign per higher geography; numeric; default=10L
#' @param polarity field name from data containing the polarity or direction for ranking values;
#'                 unquoted string with valid values RAG - High is good, RAG Low is good, High or Low; default = Polarity
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


# temp for function testing

# read in test data
library(fingertipsR)
library(dplyr)
fd <- fingertips_data(IndicatorID = c(90366,40501), AreaTypeID = 102, rank = TRUE) %>%
  group_by(IndicatorID, Sex) %>%
  filter(TimeperiodSortable == max(TimeperiodSortable) & AreaType == "County & UA") %>%
  select(IndicatorID, IndicatorName, ParentCode, ParentName, AreaCode, AreaName, Rank, Polarity, Sex, Value, AreaValuesCount)

# write to csv to test in Excel tool
# write.csv(fd,"fd_quantiles.csv")



#  data <- fd
#  values <- "Value"
#  basegeog <- "AreaCode"
#  highergeog <- "ParentCode"
#  nquantiles <- 10L
#  polarity = "Polarity"


check <- phe_quantiles(data=fd, values=Value, basegeog = AreaCode,
                       highergeog = ParentCode)



# create phe_proportion function using Wilson's method
phe_quantiles <- function(data, values, basegeog, highergeog,
                          nquantiles=10L, polarity = Polarity) {

  # check required arguments present
  if (missing(data)|missing(values)|missing(basegeog)|missing(highergeog)) {
    stop("function phe_quantiles requires at least 4 arguments: data, values, smallarea, highergeog")
  }

  # apply quotes
  values_q     <- enquo(values)
  smallarea_q  <- enquo(basegeog)
  highergeog_q <- enquo(highergeog)
  polarity_q   <- enquo(polarity)

  # error handling for valid field names and data types
  if (!(all(pull(data, !!polarity_q) %in% c("RAG - High is good", "RAG - Low is good","High","Low")))) {
    stop("polarity argument is invalid")
  } else if (!(is.numeric(pull(data, !!values_q)))) {
      stop("values argument must be a numeric field from data")
  }

  # assign field name for quantiles
  qnames <- data.frame(quantiles = c(2L,3L,4L,5L,6L,7L,8L,10L,12L,16L,20L),
                       qname     = c("Half","Tertile","Quartile","Quintile","Sextile","Septile",
                                     "Octile","Decile","Duo-decile","Hexadecile","Ventile"),
                       stringsAsFactors = FALSE) %>%
              filter(quantiles == nquantiles)

  qname <- qnames$qname

  # assign quantiles

  df <- data %>%
    group_by(!!highergeog_q, add=TRUE) %>%
    add_tally() %>%
    mutate(adj_value = if_else(!!polarity_q %in% c("RAG - Low is good","Low"),max(!!values_q,na.rm=TRUE)-!!values_q,!!values_q),
           rank = min_rank(adj_value),
           quantile = floor((nquantiles+1)-ceiling(((n+1)-rank)/(n/nquantiles))))

}



# QA against Excel Tool

Tooldata <- read.csv(".\\fd_quantiles.csv", stringsAsFactors=FALSE) %>%
  select(IndicatorID, Sex, ParentCode, AreaCode, AreaName, Value, MaxValue,
         Rank.within.Region, Tool.output...number.of.areas, Tool.Output.Decile)
QAdf <- check %>%
    filter((IndicatorID == 40501 & Sex == "Female") |
          IndicatorID == 90366 & Sex == "Female") %>%
  select(IndicatorID, Sex, ParentCode, AreaCode, AreaName, Value, adj_value,
         rank, n, quantile) %>%
  full_join(Tooldata, by = c("IndicatorID", "Sex", "ParentCode","AreaCode")) %>%
  filter(round(Value.x,2) != round(Value.y,2) |
         rank != Rank.within.Region |
         quantile != Tool.Output.Decile)

