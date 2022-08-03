#' European Standard Population 2013
#'
#' A numeric vector containing nineteen 5-year age band populations making up the 2013 European Standard Population
#' ordered from age 0-4, 5-9, 10-14 ... to ...  85-89, 90+.  Sorted by increasing age band.
#'
#' @section Notes: The 2013 European Standard Population is modelled and published by Eurostat (1) for use in the production of
#' age-standardised rates.  It uses the unweighted average 2010-based population projections of the European Union (x27)
#' and European Free Trade Association (x4) countries for the period 2011-2030 broken down into 5-year age bands from
#' age 0 - age 95+ with the 0-5 age band separated into age 0 and age 1-4.  The version provided with this package combines
#' the age 0 and age 1-4 populations into a single 0-4 age band and combines the 90-94 and 95+ populations into a single 90+ age band,
#' giving 19 age bands in total.
#'
#' @return 5000 5500 5500 5500 6000 6000 6500 7000 7000 7000 7000 6500 6000 5500 5000 4000 2500 1500 1000
#'
#' @examples
#' esp2013
#'
#' @references
#' (1) Eurostat Methodologies and Working Papers. Revision of the European Standard Population: Report of Eurostat's Taskforce, 2013. \cr
#'  \url{https://ec.europa.eu/eurostat/documents/3859598/5926869/KS-RA-13-028-EN.PDF/e713fa79-1add-44e8-b23d-5e8fa09b3f8f}
#'
#' @format A numeric vector with 19 elements
#'
"esp2013"


#' SII test datasets - Life Expectancy
#'
#' A data table of life expectancy data by area and deprivation decile
#'
#' @docType data
#'
#' @usage data(LE_data)
#'
#' @examples
#' LE_data
#'
#' @format A data table
#'
"LE_data"


#' SII test datasets - DSR
#'
#' A data table of dummy Directly Standardised Rates by deprivation quintiles
#'
#' @docType data
#'
#'
#' @usage data(DSR_data)
#'
#' @examples
#' DSR_data
#'
#' @format A data table
#'
"DSR_data"


#' SII test datasets - Prevalence
#'
#' A data table of example prevalence data by area and deprivation decile
#'
#' @docType data
#'
#' @usage data(prevalence_data)
#'
#' @examples
#' prevalence_data
#'
#' @format A data table
#'
"prevalence_data"
