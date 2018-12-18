#' phe_life_expectancy
#'
#' Compute life expectancy from a given age and its standard error
#'
#' @param data data.frame or tbl containg the deaths and population data
#' @param deaths field name from data containing the number of deaths within age
#'   band; unquoted string; no default
#' @param population field name from data containing the population within age
#'   band; unquoted string; no default
#' @param startage field name from data containing the startage for the age
#'   band; no default
#' @param age_contents vector; describes the contents of startage in the
#'   ascending order. This vector will be used to check whether each group
#'   contains the complete set of age bands for the calculation to occur and
#'   will apply the correct order to the data
#' @param le_age the age band to return the life expectancy for. The default is
#'   "all", where the function returns the life expectancy values appended onto
#'   the input table. Any other value (or vector of values) must be age bands
#'   described by the age_contents input
#' @param type type of output; can be "standard" or "full" (which contains
#'   details added details on the calculation within the dataframe); quoted
#'   string; default standard
#' @inheritParams phe_dsr
#' @import dplyr
#' @examples
#' library(dplyr)
#'
#' ## A simple example
#' df <- data.frame(startage = c(0L, 1L, 5L, 10L, 15L, 20L, 25L, 30L, 35L, 40L, 45L, 50L, 55L,
#'                               60L, 65L, 70L, 75L, 80L, 85L, 90L),
#'                  pops = c(7060L, 35059L, 46974L, 48489L, 43219L, 38561L, 46009L, 57208L,
#'                           61435L, 55601L, 50209L, 56416L, 46411L, 39820L, 37978L,
#'                           37039L, 33288L, 23306L, 11936L, 11936L),
#'                  deaths = c(17L, 9L, 4L, 8L, 20L, 15L, 24L, 33L, 50L, 71L, 100L, 163L,
#'                             263L, 304L, 536L, 872L, 1390L, 1605L, 1936L, 1937L))
#' phe_life_expectancy(df, deaths, pops, startage)
#'
#' ## OR
#'
#' phe_life_expectancy(df, deaths, pops, startage, le_age = c(5, 25), type = "full")
#'
#' ## Unordered age bands example
#' df <- data.frame(startage = c("0", "1-4", "5-9", "10 - 14", "15 - 19", "20 - 24", "25 - 29",
#'                               "30 - 34", "35 - 39", "40 - 44", "45 - 49", "50 - 54",
#'                               "55 - 59", "60 - 64", "65 - 69", "75 - 79", "80 - 84",
#'                               "85 - 89", "90 +", "70 - 74"),
#'                  pops = c(7060L, 35059L, 46974L, 48489L, 43219L, 38561L, 46009L, 57208L,
#'                           61435L, 55601L, 50209L, 56416L, 46411L, 39820L, 37039L,
#'                           23306L, 11936L, 11936L, 37978L, 33288L),
#'                  deaths = c(17L, 9L, 4L, 8L, 20L, 15L, 24L, 33L, 50L, 71L, 100L, 163L,
#'                             263L, 304L, 872L, 1605L, 1936L, 1937L, 536L, 1390L))
#' phe_life_expectancy(df, deaths, pops, startage,
#'                     age_contents = c("0", "1-4", "5-9",
#'                                      "10 - 14", "15 - 19",
#'                                      "20 - 24", "25 - 29",
#'                                      "30 - 34", "35 - 39",
#'                                      "40 - 44", "45 - 49",
#'                                      "50 - 54", "55 - 59",
#'                                      "60 - 64", "65 - 69",
#'                                      "70 - 74", "75 - 79",
#'                                      "80 - 84", "85 - 89",
#'                                      "90 +"))
#'
# ## A grouped data example
#' df <- data.frame(area = c(rep("Area 1", 20), rep("Area 2", 20)),
#'                  startage = rep(c(0L, 1L, 5L, 10L, 15L, 20L, 25L, 30L, 35L, 40L, 45L, 50L, 55L,
#'                                   60L, 65L, 70L, 75L, 80L, 85L, 90L), 2),
#'                  pops = rep(c(7060L, 35059L, 46974L, 48489L, 43219L, 38561L, 46009L, 57208L,
#'                               61435L, 55601L, 50209L, 56416L, 46411L, 39820L, 37978L,
#'                               37039L, 33288L, 23306L, 11936L, 11936L), 2),
#'                  deaths = rep(c(17L, 9L, 4L, 8L, 20L, 15L, 24L, 33L, 50L, 71L, 100L, 163L,
#'                                 263L, 304L, 536L, 872L, 1390L, 1605L, 1936L, 1937L), 2))
#' df %>%
#'        group_by(area) %>%
#'        phe_life_expectancy(deaths, pops, startage)
#'
#' @author Sebastian Fox, \email{sebastian.fox@@phe.gov.uk}
#' @export
#'
#' @family PHEindicatormethods package functions
phe_life_expectancy <- function(data, deaths, population, startage,
                                age_contents = c(0L, 1L, 5L, 10L, 15L,
                                                 20L, 25L, 30L, 35L, 40L,
                                                 45L, 50L, 55L, 60L, 65L,
                                                 70L, 75L, 80L, 85L, 90L),
                                le_age = "all", type = "standard", confidence = 0.95) {

  # check required arguments present
  if (missing(data) | missing(deaths) | missing(population) | missing(startage)) {
    stop("function life_expectancy requires at least 4 arguments: data, deaths, population, startage")
  }

  # check for 20 age bands
  if (length(age_contents) != 20) stop("this function requires 20 age bands to work (0, 1-4, 5-9, 10-14, ..., 85-89, 90+)")

  # check that min age  is 0
  stripped_age_contents <- as.integer(sub("\\D*(\\d+).*", "\\1", age_contents))
  if (stripped_age_contents[1] != 0) stop("first age band in age_contents must be 0")

  # check age_contents is ascending
  if (!identical(stripped_age_contents, sort(stripped_age_contents))) stop(paste("age_contents doesn't appear to be in ascending order; the following age bands appear out of position:",
                                                                                 paste(age_contents[stripped_age_contents != sort(stripped_age_contents)],
                                                                                       collapse = ", ")))

  # apply quotes
  deaths <- enquo(deaths)
  population <- enquo(population)
  startage <- enquo(startage)

  # check on confidence limit requirements
  if ((confidence < 0.9) | (confidence > 1 & confidence < 90) | (confidence > 100)) {
    stop("confidence level must be between 90 and 100 or between 0.9 and 1")
  }

  # compare startage field with age_contents
  age_bands <- data %>%
          pull(!!startage) %>%
          unique() %>%
          sort()

  if (!identical(as.character(age_bands), as.character(sort(age_contents)))) stop("the contents in the startage field do not match the contents of the age_contents vector")

  #calculate start age for each age band
  data <- data %>%
          mutate(startage_2b_removed = as.integer(sub("\\D*(\\d+).*", "\\1", !!startage)))

  # order the data by (group variables) and start age
  if (length(group_vars(data)) > 0) {
          data <- data %>%
                  arrange(startage_2b_removed,
                          .by_group = TRUE)
  } else {
          data <- data %>%
                  arrange(startage_2b_removed)
  }

  # check for negative deaths
  negative_deaths <- data %>%
          filter(!!deaths < 0) %>%
          count() %>%
          filter(n != 0) %>%
          select(-n)
  if (nrow(negative_deaths) > 0) {
          warning("some age bands have negative deaths; outputs have been suppressed to NAs")
          if (length(group_vars(data)) > 0) {
                  negative_deaths <- negative_deaths %>%
                          left_join(data, by = group_vars(data)) %>%
                          mutate(value = NA,
                                 lowercl = NA,
                                 uppercl = NA)
                  # remove areas with deaths < 0 in any age band
                  data <- data %>%
                          anti_join(negative_deaths, by = group_vars(data))
          } else {
                  data <- data %>%
                          mutate(value = NA,
                                 lowercl = NA,
                                 uppercl = NA)
                  return(data)
          }



  }
  # check for less than or equal to zero pops
  negative_pops <- data %>%
          filter(!!population <= 0) %>%
          count() %>%
          filter(n != 0) %>%
          select(-n)
  if (nrow(negative_pops) > 0) {
          warning("some age bands have a zero or less population; outputs have been suppressed to NAs")
          if (length(group_vars(data)) > 0) {
                  negative_pops <- negative_pops %>%
                          left_join(data, by = group_vars(data)) %>%
                          mutate(value = NA,
                                 lowercl = NA,
                                 uppercl = NA)
                  # remove areas with pops <= 0 in any age band
                  data <- data %>%
                          anti_join(negative_pops, by = group_vars(data))
          } else {
                  data <- data %>%
                          mutate(value = NA,
                                 lowercl = NA,
                                 uppercl = NA)
                  return(data)
          }



  }
  # check for all rows per group
  number_age_bands <- length(age_contents)
          incomplete_areas <- data %>%
          count() %>%
          ungroup() %>%
          filter(n != number_age_bands) %>%
          select(-n)
  if (nrow(incomplete_areas) > 0) {
          warning("some groups contain a different number of rows than the number of age groups described by the age_contents input; life expectancy cannot be calculated for these. These groups will contain NAs.")

          # Insert NAs into output fields to be row bound to the final output at end
          if (length(group_vars(data)) > 0) {
                  incomplete_areas <- incomplete_areas %>%
                          left_join(data, by = group_vars(data)) %>%
                          mutate(value = NA,
                                 lowercl = NA,
                                 uppercl = NA)

                  # remove areas with incomplete number of age bands
                  data <- data %>%
                          anti_join(incomplete_areas, by = group_vars(data))
          } else {
                  data <- data %>%
                          mutate(value = NA,
                                 lowercl = NA,
                                 uppercl = NA)
                  return(data)
          }

  }

  # check for deaths > pops
  deaths_more_than_pops <- data %>%
          filter(!!deaths > !!population) %>%
          count() %>%
          filter(n != 0) %>%
          select(-n)
  if (nrow(deaths_more_than_pops) > 0) {
          warning("some age bands have more deaths than population; outputs have been suppressed to NAs")
          if (length(group_vars(data)) > 0) {
                  deaths_more_than_pops <- deaths_more_than_pops %>%
                          left_join(data, by = group_vars(data)) %>%
                          mutate(value = NA,
                                 lowercl = NA,
                                 uppercl = NA)
                  # remove areas with deaths > pops in any age band
                  data <- data %>%
                          anti_join(deaths_more_than_pops, by = group_vars(data))
          } else {
                  data <- data %>%
                          mutate(value = NA,
                                 lowercl = NA,
                                 uppercl = NA)
                  return(data)
          }



  }

  # check for pops <= 5000
  total_pops <- data %>%
          summarise(total_pop = sum(!!population)) %>%
          filter(total_pop <= 5000) %>%
          select(-total_pop)
  if (nrow(total_pops) > 0) {
          warning("some groups have a total population of less than 5,000; outputs have been suppressed to NAs")
          if (length(group_vars(data)) > 0) {
                  total_pops <- total_pops %>%
                          left_join(data, by = group_vars(data)) %>%
                          mutate(value = NA,
                                 lowercl = NA,
                                 uppercl = NA)

                  # remove areas with pops <= 5000
                  data <- data %>%
                          anti_join(total_pops, by = group_vars(data))
          } else {
                  data <- data %>%
                          mutate(value = NA,
                                 lowercl = NA,
                                 uppercl = NA)
                  return(data)
          }

  }
  suppressed_data <- bind_rows(negative_deaths,
                               negative_pops,
                               incomplete_areas,
                               deaths_more_than_pops,
                               total_pops) %>%
          unique()

  # scale confidence level
  if (confidence >= 90) {
    confidence <- confidence/100
  }
  z <- qnorm(confidence + (1 - confidence)/2)
  data$group_id_2b_removed <- data %>%
    group_indices()
  data <- data %>%
    mutate(id_2b_removed = row_number(),
           ni_2b_removed = as.numeric(lead(startage_2b_removed) - startage_2b_removed),
           ai_2b_removed = case_when(
                   startage_2b_removed == 0 ~ 0.1,
                   TRUE ~ 0.5),
           M_2b_removed = !!deaths / !!population,
           ni_2b_removed = case_when(
             is.na(ni_2b_removed) ~ 2 / M_2b_removed,
             TRUE ~ ni_2b_removed),
           qi_2b_removed = case_when(
             id_2b_removed < number_age_bands ~ M_2b_removed * ni_2b_removed / (1 + M_2b_removed * ni_2b_removed * (1 - ai_2b_removed)),
             TRUE ~ 1),
           p_2b_removed = 1 - qi_2b_removed,
           l_2b_removed = case_when(
             id_2b_removed == 1 ~ 1e5,
             TRUE ~ 1),
           p1_2b_removed = lag(p_2b_removed,
                               default = 1),
           l_2b_removed = case_when(
             id_2b_removed != 1 ~ cumprod(l_2b_removed * p1_2b_removed),
             TRUE ~ l_2b_removed),
           di_2b_removed = l_2b_removed - lead(l_2b_removed, default = 0),
           Li_2b_removed = case_when(
             id_2b_removed < number_age_bands ~ ni_2b_removed * (lead(l_2b_removed) + (ai_2b_removed * di_2b_removed)),
             TRUE ~ l_2b_removed / M_2b_removed),
           Ti_2b_removed = rev(cumsum(rev(Li_2b_removed))),
           ei = case_when(
             l_2b_removed == 0 ~ 0,
             TRUE ~ Ti_2b_removed / l_2b_removed),
           spi_2b_removed = case_when(
             di_2b_removed == 0 ~ 0,
             TRUE ~ (qi_2b_removed ^ 2) * (1 - qi_2b_removed) / !!deaths),
           spi_2b_removed = case_when(
             id_2b_removed == number_age_bands ~ M_2b_removed * (1 - M_2b_removed) / !!population,
             TRUE ~ spi_2b_removed),
           W_spi_2b_removed = case_when(
             id_2b_removed < number_age_bands ~ spi_2b_removed * (l_2b_removed ^ 2) * (((1 - ai_2b_removed) * ni_2b_removed + lead(ei)) ^ 2),
             TRUE ~ (l_2b_removed ^ 2) / (M_2b_removed ^ 4) * spi_2b_removed),
           STi_2b_removed = rev(cumsum(rev(W_spi_2b_removed))),
           SeSE_2b_removed = sqrt(STi_2b_removed / (l_2b_removed ^ 2)),
                lowercl = ei - z * SeSE_2b_removed,
                uppercl = ei + z * SeSE_2b_removed) %>%
    select(-ends_with("_2b_removed")) %>%
    rename(value = ei)

  data$value[data$value == Inf] <- NA
  data$lowercl[is.nan(data$lowercl)] <- NA
  data$uppercl[is.nan(data$uppercl)] <- NA

  if (nrow(suppressed_data) > 0) data <- bind_rows(data, suppressed_data)

  if (length(le_age) == 1) {
          if (le_age != "all") {
                  if (sum(age_contents %in% le_age) == 0) {
                          warning("le_age not in the vector described by age_contents; all life expectancies will be returned")
                  } else {
                          data <- data %>%
                                  filter(!!startage %in% le_age)
                  }
          }
  } else {
          if (sum(age_contents %in% le_age) == 0) {
                  warning("le_age not in the vector described by age_contents; all life expectancies will be returned")
          } else {
                  data <- data %>%
                          filter(!!startage %in% le_age)
          }
  }

  if (type == "full") {
          data <- data %>%
                  mutate(confidence = confidence,
                         statistic = paste("life expectancy at", !!startage),
                         method = "Chiang, using Silcocks et al for confidence limits")
  }
  return(data)

}
