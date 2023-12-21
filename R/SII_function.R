#' Calculate Slope Index of Inequality using phe_sii
#'
#' @description
#' `phe_sii()`returns the slope index of inequality (SII) statistic for each
#' subgroup of the inputted dataframe, with lower and upper confidence limits
#' based on the specified confidence.
#'
#' @details
#'
#' The Relative Index of Inequality (RII) can also be returned via an optional
#' argument.
#'
#' The SII and RII are two measures of health inequality. They show the relation
#' between the level of health or frequency of a health problem in different
#' population groups and the ranking of these groups on the social scale.
#'
#' The input dataframe should be grouped before passing to the function if an
#' SII/RII for each subgroup is required, and quantiles ordered from least to
#' most advantaged.
#'
#' @section Calculation:
#'
#' The SII is calculated using linear regression (1). To allow for differences
#' in population size between quantiles (e.g. deprivation deciles), each is
#' given a rank score (or relative rank) based on the midpoint of its range in
#' the cumulative distribution of the total area population. The quantiles are
#' first ordered (e.g from 1 most deprived to 10 least deprived for deprivation
#' deciles). If quantile 1 then contains 12 percent of the total population, its
#' relative rank is \code{0.12/2=0.6}. If quantile 2 includes 10 percent of the
#' population, its relative rank is \code{0.12+(0.10/2)=0.17}. A square root
#' transformation is applied to the regression to account for heteroskedasticity
#' (the tendancy for the variances of the quantile values to be related to the
#' size of the values, ie larger values will tend to have larger variances). A
#' regression model is fitted to the transformed data:  \eqn{Y * \sqrt a = \sqrt
#' a + b * \sqrt a}, where Y is the value of the indicator for the quantile, a
#' is the proportion of the total population in the quantile and b is the
#' relative rank. The SII is the gradient of the resulting fitted line, and
#' could be positive or negative according to the indicator polarity. Since the
#' relative ranks, by definition, range from 0 to 1, the SII is the difference
#' between the fitted value at  \code{x=1} and  \code{x=0}. The RII is the ratio
#' of the fitted value at  \code{x=1,Y1} and the fitted value at \code{x=0,Y0}.
#' which can be calculated as:  \code{RII = (Y0 + SII)/Y0}
#'
#' @section Function arguments:
#'
#' The indicator type can be specified via the \code{value_type} parameter. It
#' is recommended that rate and proportion indicators are transformed for
#' calculation of the SII using the \code{transform} parameter set to TRUE. This
#' will perform a log transformation for rates, and logit for proportions, and
#' return outputs transformed back to the original units of the indicator.
#' Where the \code{transform} parameter is set to FALSE transformations are
#' applied to the indicator value and its confidence limits before calculating
#' the standard error in cases where the confidence interval around the indicator
#' value is likely to be non-symmetric. If the standard error is supplied directly
#' to the function from the input dataset, this is used instead of calculating
#' one from the indicator confidence limits.
#'
#' @section Warning:
#'
#' The SII calculation assumes a linear relationship between indicator value and
#' quantile. Where this is not the case the transform option should be considered.
#' Small populations within quantiles can make the SII unstable. This
#' function does not include checks for linearity or stability; it is the user's
#' responsibility to ensure the input data is suitable for the SII calculation.
#'
#' @section Notes:
#'
#' This function is using nest and unnest functions from tidyr version 1.0.0.
#'
#' @param data data.frame containing the required input fields, pre-grouped if an SII is required for
#'        each subgroup; unquoted string; no default
#' @param quantile field name within data that contains the quantile label (e.g. decile). The number
#'        of quantiles should be between 5 and 100; unquoted string; no default
#' @param population field name within data that contains the quantile populations (ie, denominator).
#'        Non-zero populations are required for all quantiles to calculate SII for an area;
#'        unquoted string; no default
#' @param x (for indicators that are proportions) field name within data that contains
#'        the members of the population with the attribute of interest (ie, numerator). This will be
#'        divided by population to calculate a proportion as the indicator value
#'        (if value field is not provided); unquoted string; no default
#' @param value field name within data that contains the indicator value (this does not need to be supplied
#'        for proportions if count and population are given); unquoted string; no default
#' @param value_type indicates the indicator type (1 = rate, 2 = proportion, 0 = other);
#'        integer; default 0
#' @param lower_cl field name within data that contains 95 percent lower confidence limit
#'        of indicator value (to calculate standard error of indicator value). This field is needed
#'        if the se field is not supplied; unquoted string; no default
#' @param upper_cl field name within data that contains 95 percent upper confidence limit
#'        of indicator value (to calculate standard error of indicator value). This field is needed
#'        if the se field is not supplied; unquoted string; no default
#' @param se field name within data that contains the standard error of the indicator
#'        value. If not supplied, this will be calculated from the 95 percent lower and upper confidence
#'        limits (i.e. one or the other of these fields must be supplied); unquoted string; no default
#' @param multiplier factor to multiply the SII and SII confidence limits by (e.g. set to 100 to return
#'        prevalences on a percentage scale between 0 and 100). If the multiplier is negative, the
#'        inverse of the RII is taken to account for the change in polarity; numeric; default 1;
#' @param repetitions number of random samples to perform to return confidence interval of SII (and RII).
#'        Minimum is 1000, no maximum (though the more repetitions, the longer the run time);
#'        numeric; default 100,000
#' @param confidence confidence level used to calculate the lower and upper confidence limits of SII,
#'        expressed as a number between 0.9 and 1, or 90 and 100. It can be a vector of 0.95 and 0.998,
#'        for example, to output both 95 percent and 99.8 percent CIs; numeric; default 0.95
#' @param rii option to return the Relative Index of Inequality (RII) with associated confidence limits
#'        as well as the SII; logical; default FALSE
#' @param intercept option to return the intercept value of the regression line (y value where x=0);
#'        logical; default FALSE
#' @param transform option to transform input data prior to calculation of the SII, where there is not
#'        a linear relationship between the indicator values and the quantile; logical; default FALSE
#' @param reliability_stat option to carry out the SII confidence interval simulation 10 times instead
#'        of once and return the Mean Average Difference between the first and subsequent samples (as a
#'        measure of the amount of variation). Warning: this will significantly increase run time of the
#'        function and should first be tested on a small number of repetitions; logical; default FALSE
#' @param type "full" output includes columns in the output dataset specifying the parameters the user
#'        has input to the function (value_type, multiplier, CI_confidence, CI_method); character string
#'        either "full" or "standard"; default "full"
#'
#' @references
#' (1) Low A & Low A. Measuring the gap: quantifying and comparing local health inequalities.
#' Journal of Public Health; 2004;26:388-395. \cr \cr
#'
#' @import dplyr
#' @import broom
#' @importFrom rlang quo_text
#' @importFrom purrr map
#' @importFrom tidyr nest unnest spread
#' @importFrom stats rnorm qnorm lm
#' @importFrom tidyselect where
#' @importFrom rlang := .data .env
#'
#' @examples
#' library(dplyr)
#'
#' data <- data.frame(area = c(rep("Area1", 10), rep("Area2", 10)),
#'                    decile = c(1:10, 1:10),
#'                    population = c(7291, 7997, 6105, 7666, 5790, 6934, 5918, 5974, 7147, 7534, 21675,
#'                                   20065, 19750, 24713, 20112, 19618, 22408, 19752, 18939, 19312),
#'                    value = c(75.9, 78.3, 83.8, 83.6, 80.5, 81.1, 81.7, 84.2, 80.6, 86.3, 70.5,
#'                               71.6, 72.5, 73.5, 73.1, 76.2, 78.7, 80.6, 80.9, 80),
#'                    lowerCL = c(72.7,75.3,80.9,80.2,77.1,78,79,81.4,75.8,83.2,
#'                                70.1,71.1,72,73.1, 72.7, 75.7, 78.2,80.1,80.4,79.5),
#'                    upperCL = c(79.1,81.4,86.8,87.1,83.8,84.2,84.4,86.9,85.4,
#'                                 89.4,71,72.1,73.2,73.7,75.8,78.8,79.8,81.2,81.3,80.9),
#'                    StandardError = c(1.64,1.58,1.51,1.78,1.7,1.56,1.37,1.4,2.43,
#'                                      1.57,0.23,0.26,0.3,0.16,0.79,0.78,0.4,0.28,0.23,0.35)
#'                    )
#'
#'
#' # Run SII function on the two areas in the data
#' phe_sii(group_by(data, area),
#'         decile,
#'         population,
#'         value_type = 0, # default normal distribution
#'         value = value,
#'         lower_cl = lowerCL,
#'         upper_cl = upperCL,
#'         confidence = 0.95,
#'         rii = TRUE,
#'         type = "standard")
#'
#' # Supplying the standard error instead of the indicator 95 percent confidence limits
#' # gives the same result
#' phe_sii(group_by(data, area),
#'         decile,
#'         population,
#'         value_type = 0,
#'         value = value,
#'         se = StandardError,
#'         confidence = 0.95,
#'         rii = TRUE,
#'         type = "standard")
#'
#' # multiple confidence intervals
#' phe_sii(group_by(data, area),
#'         decile,
#'         population,
#'         value_type = 0,
#'         value = value,
#'         se = StandardError,
#'         confidence = c(0.95, 0.998),
#'         repetitions = 10000,
#'         rii = TRUE,
#'         type = "standard")
#'
#' @export
#'
#' @return The SII with lower and upper confidence limits for each subgroup of
#'   the inputted data.frame.
#'
#' @family PHEindicatormethods package functions

phe_sii <- function(data, quantile, population,  # compulsory fields
                    x = NULL,                    # optional fields
                    value = NULL,
                    value_type = 0,
                    lower_cl = NULL,
                    upper_cl = NULL,
                    se = NULL,
                    multiplier = 1,
                    repetitions = 100000,
                    confidence = 0.95,
                    rii = FALSE,
                    intercept = FALSE,
                    transform = FALSE,
                    reliability_stat = FALSE,
                    type = "full") {

        # Part 1 - Checks on input data ---------------------------------------------

        if (missing(data)| missing(quantile)| missing(population)) {
          stop("function phe_sii requires the arguments: data, quantile, population")
        }
        if (missing(value) & missing(x)) {
          stop("function phe_sii requires value field, or x field if indicator is a proportion of population")
        }
        if (missing(se) & (missing(upper_cl) | missing(lower_cl))) {
          stop("function phe_sii requires either lower_cl and upper_cl fields, or se field")
        }
        if (!(value_type %in% c(0,1,2))) {
          stop("value_type should be 0, 1 or 2")
        }
        if (!(class(multiplier) %in% c("numeric", "integer") & class(repetitions) %in% c("numeric", "integer") & class(confidence) %in% c("numeric", "integer"))) {
          stop("multiplier, repetitions and confidence inputs should be numeric")
        }
        if (repetitions < 1000) {
          stop("number of repetitions must be 1000 or greater. Default is 100,000")
        }
        # if transform is true then value type must be rate or proportion
        if (transform == TRUE & value_type == 0) {
          stop("value_type should be 1 or 2 when transform is true")
        }
        # if transform is true then se cannot be provided
        if (transform == TRUE & !(missing(se))) {
          stop("function phe_sii requires se to be missing when transform is true")
        }
        # if transform is true then upper and lower cls must be provided
        if (transform == TRUE & (missing(upper_cl) | missing(lower_cl))) {
          stop("function phe_sii requires lower_cl and upper_cl fields when transform is true")
        }
        # check on confidence limit requirements
        if (any(confidence < 0.9) | (any(confidence > 1) & any(confidence < 90)) | any(confidence > 100)) {
            stop("all confidence levels must be between 90 and 100 or between 0.9 and 1")
        }

        # Use NSE on inputs - apply quotes
        quantile = enquo(quantile)
        population = enquo(population)
        if(!missing(x)) {x = enquo(x)}
        if(!missing(value)) {value = enquo(value)}
        if(!missing(se)) {se = enquo(se)}
        if(!missing(lower_cl)) {lower_cl = enquo(lower_cl)}
        if(!missing(upper_cl)) {upper_cl = enquo(upper_cl)}

        # scale confidence level
        confidence[confidence >= 90] <- confidence[confidence >= 90] / 100

        # check for non numeric inputs
        if(!(class(pull(data, {{ population }})) %in% c("numeric", "integer")
            & ifelse(rlang::quo_text(x) %in% names(data), (class(pull(data, {{ x }})) %in% c("numeric", "integer")), TRUE)
            & ifelse(rlang::quo_text(value) %in% names(data), (class(pull(data, {{ value }})) %in% c("numeric", "integer")), TRUE)
            & ifelse(rlang::quo_text(se) %in% names(data), (class(pull(data, {{ se }})) %in% c("numeric", "integer")), TRUE)
            & ifelse(rlang::quo_text(lower_cl) %in% names(data), (class(pull(data, {{ lower_cl }})) %in% c("numeric", "integer")), TRUE)
            & ifelse(rlang::quo_text(upper_cl) %in% names(data), (class(pull(data, {{ upper_cl }})) %in% c("numeric", "integer")), TRUE))) {
                stop("some input fields in data.frame are non-numeric")
        }

        # check for zero or negative populations
        negative_pops <- data %>%
                filter({{ population }} <= 0 | is.na({{ population }}))

                if (nrow(negative_pops) > 0) {
                        stop("some groups have a zero, negative or missing population")
        }

        # check for negative/missing standard errors
        if(rlang::quo_text(se) %in% names(data)) {
        negative_se <- data %>%
                filter({{ se }} < 0 | is.na({{ se }}))

                if (nrow(negative_se) > 0) {
                        stop("negative or missing standard errors in input dataset")
                        }
        }

        # check for missing confidence limits
        if(rlang::quo_text(lower_cl) %in% names(data) & rlang::quo_text(upper_cl) %in% names(data)) {
          negative_cl <- data %>%
            filter(is.na({{ lower_cl }}) | is.na({{ upper_cl }}))

          if (nrow(negative_cl) > 0) {
            stop("missing lower or upper confidence limits in input dataset")
          }
        }

        # checks on PROPORTIONS
        if(value_type == 2) {

            # check for proportions outside (0,1) range
            if(rlang::quo_text(value) %in% names(data)) {
              invalid_prop <- data %>%
                filter({{ value }} < 0 | {{ value }} > 1)

              if (nrow(invalid_prop) > 0) {
                stop("value proportions are not all between 0 and 1")
              }
            }

            # check for lower and upper CLs outside (0,1) range
            if(rlang::quo_text(lower_cl) %in% names(data) &
               rlang::quo_text(upper_cl) %in% names(data)) {
              invalid_prop_cl <- data %>%
                filter({{ lower_cl }} < 0 | {{ lower_cl }} > 1 |
                         {{ upper_cl }} < 0 | {{ upper_cl }} > 1)

              if (nrow(invalid_prop_cl) > 0) {
                stop("confidence limit proportions are not all between 0 and 1")
              }
            }

            # check for zero or negative counts
            if(!(rlang::quo_text(value) %in% names(data)) &
               rlang::quo_text(x) %in% names(data)) {
              negative_x <- data %>%
                filter({{ x }} <= 0)

              if (nrow(negative_x) > 0) {
                stop("some groups have a zero or negative count x")
              }
          }
        }

        # Part 2 - Start calculations ---------------------------------------------

        # extract grouping variables of input dataset (if any)
        grouping_variables <- group_vars(data)

        # Convert factors to character
        data <- data %>%
                 ungroup() %>%
                 mutate(across(where(is.factor), as.character)) %>%
                 group_by(!!! syms(c(grouping_variables)))

        # Extract vector of quantiles and save the number to "no_quantiles"
        quantile_list <- unique(select(ungroup(data), {{ quantile }}))
        no_quantiles <- nrow(quantile_list)

        # Output warning on number of quantiles inputted
        if (no_quantiles < 5 | no_quantiles > 100) {
                stop("Number of quantiles must be between 5 and 100")
        } else if (no_quantiles > 10) {
                warning("WARNING: Small values can make SII unstable when using a large number of quantiles")
        }

        # Remove records with missing essential data
        if (rlang::quo_text(se) %in% names(data)) {

        valid_complete <- data %>%
                             filter({{ population }} > 0,
                                    !is.na({{ se }}))
        } else if (rlang::quo_text(lower_cl) %in% names(data) &
                   rlang::quo_text(upper_cl) %in% names(data)) {

        valid_complete <- data %>%
                                filter({{ population }} > 0,
                                       !is.na({{ lower_cl }}), !is.na({{ upper_cl }}))
        }

        # Not all quantiles may have data for each grouping
        # Start by counting the number of quantiles each area has data for -
        # exclude any areas with missing data (SII cannot be calculated)
        valid_areas <- valid_complete %>%
                         summarise(n = length(unique({{ quantile }}))) %>%
                         filter(n == no_quantiles)

        # Create table of areas to calculate SII for
        valid_deciles <- valid_areas %>%
                         merge(quantile_list, # Merge on list of quantiles
                              all.x = TRUE,
                              all.y = TRUE)

        if (nrow(valid_deciles) != nrow(data)) {
                warning("WARNING: some records have been removed due to incomplete or invalid data")
        }


        # join provided data to valid decile table
        pops_prep <- left_join(valid_deciles, data,
                            by = c(grouping_variables, rlang::quo_text(quantile))) %>%
                     group_by(!!! syms(c(grouping_variables, rlang::quo_text(quantile)))) %>%
                     arrange(!!! syms(c(grouping_variables, rlang::quo_text(quantile))))

        # Calculate indicator value (if not supplied in input data) as proportion for each
        # quantile (x/population)

        if (rlang::quo_text(value) %in% names(pops_prep)) {
                pops_prep <- mutate(pops_prep, value = {{ value }})
        } else if (value_type == 2) {
                pops_prep <- mutate(pops_prep, value = {{ x }} / {{ population }})
        }

        # Transform value if value is a rate or proportion
         pops_prep <- pops_prep %>%
                mutate(value = ifelse(value_type == 1,
                                      log(.data$value),
                               ifelse(value_type == 2,
                                      log(.data$value / (1 - .data$value)),
                                      .data$value)))

        # Transform lower and upper confidence limits in the case of a rate or proportion
         if (rlang::quo_text(lower_cl) %in% names(pops_prep) &
             rlang::quo_text(upper_cl) %in% names(pops_prep)) {

         pops_prep <- pops_prep %>%
                mutate(lower_cl = ifelse(value_type == 0, {{ lower_cl }},
                                      ifelse(value_type == 1, log({{ lower_cl }}),
                                         ifelse(value_type == 2, log({{ lower_cl }} / (1 - {{ lower_cl }})),
                                                NA))),
                       upper_cl = ifelse(value_type == 0, {{ upper_cl }},
                                        ifelse(value_type == 1, log({{ upper_cl }}),
                                                ifelse(value_type == 2, log({{ upper_cl }} / (1 - {{ upper_cl }})),
                                         NA))))
         }

        # Calculate standard error (if not supplied in input data), from lower and upper CLs
        z <- stats::qnorm(0.975) # hard-coded at 95% confidence

        if (rlang::quo_text(se) %in% names(pops_prep)) {
                pops_prep <- mutate(pops_prep,
                                    se_calc = {{ se }})
        } else {
                pops_prep <- mutate(pops_prep,
                                    se_calc = (upper_cl - lower_cl) / z / 2)
        }

        # Calculate a and b vals
        pops_prep_ab <- pops_prep %>%
                group_by(!!! syms(grouping_variables)) %>%
                mutate(a_vals = {{ population }}/ sum({{ population }}), # Proportion of total population of subgroup
                       b_vals = FindXValues({{ population }}, no_quantiles))

        # Calculate sqrt(a), bsqrt(a) and un-transformed y value for regression
        if(transform == FALSE) {
          pops_prep_ab <- pops_prep_ab %>%
                group_by(!!! syms(c(grouping_variables, rlang::quo_text(quantile)))) %>%
                mutate(sqrt_a = sqrt(.data$a_vals),
                       b_sqrt_a = .data$b_vals * .data$sqrt_a,
                       value_transform = ifelse(value_type == 1, exp(.data$value),
                                                ifelse(value_type == 2,
                                                       exp(.data$value) / (1 + exp(.data$value)),
                                                       .data$value)),
                       yvals = .data$sqrt_a * .data$value_transform)
        } else {
          pops_prep_ab <- pops_prep_ab %>%
          group_by(!!! syms(c(grouping_variables, rlang::quo_text(quantile)))) %>%
          mutate(sqrt_a = sqrt(.data$a_vals),
                  b_sqrt_a = .data$b_vals * .data$sqrt_a,
                  value_transform = .data$value,
                  yvals = .data$sqrt_a * .data$value_transform)
        }

        # calculate confidence interval for SII via simulation
        # Repeat this 10 times to get a "variability" measure if requested

        # Nest data (different argument needed for grouped vs. ungrouped dataset)
        if(length(grouping_variables) == 0) {
            popsSII_model <- pops_prep_ab %>%
                tidyr::nest(data = everything())
        } else {
            popsSII_model <- pops_prep_ab %>%
                group_by(!!! syms(grouping_variables)) %>%
                tidyr::nest()
        }

        # Different nest() argument needed for ungrouped dataset
        if(length(grouping_variables) == 0) {
            sim_CI <- pops_prep_ab %>%
                tidyr::nest(data = everything()) %>%
                mutate(CI_params = purrr::map(data, ~ SimulationFunc(data = .,
                                                                     .data$value,
                                                                     value_type,
                                                                     .data$se_calc,
                                                                     repetitions,
                                                                     confidence,
                                                                     multiplier,
                                                                     .data$sqrt_a,
                                                                     .data$b_sqrt_a,
                                                                     rii,
                                                                     transform,
                                                                     reliability_stat)))
        } else {
            sim_CI <- pops_prep_ab %>%
                group_by(!!! syms(grouping_variables)) %>%
              tidyr::nest() %>%
              mutate(CI_params = purrr::map(data, ~ SimulationFunc(data = .,
                                                                   .data$value,
                                                                   value_type,
                                                                   .data$se_calc,
                                                                   repetitions,
                                                                   confidence,
                                                                   multiplier,
                                                                   .data$sqrt_a,
                                                                   .data$b_sqrt_a,
                                                                   rii,
                                                                   transform,
                                                                   reliability_stat)))
        }



        # Perform regression to calculate SII and extract model parameters

        popsSII_model <- popsSII_model %>%
          # perform linear model
          mutate(model = purrr::map(data, function(df)
            stats::lm(yvals ~ sqrt_a + b_sqrt_a - 1, data = df))) %>%
          # extract model coefficients
          mutate(model = purrr::map(.data$model, broom::tidy)) %>%
          tidyr::unnest("model") %>%
          # remove unecessary fields
          select(!c("std.error", "statistic", "p.value")) %>%
          # create columns for each parameter
          tidyr::pivot_wider(names_from = "term",
                             values_from = "estimate")


        # Format results according to whether transform = T/F

        if(transform == FALSE) { #no anti-transform needed
          # Extract SII and RII values
          popsSII_model <- popsSII_model %>%
            mutate(sii = multiplier * .data$b_sqrt_a,
                   rii = (.data$sqrt_a + .data$b_sqrt_a)/.data$sqrt_a,
                   intercept = .data$sqrt_a) %>%
            # Take inverse of RII if multiplier is negative
            mutate(rii = ifelse(multiplier < 0, 1 / rii, rii)) %>%
            # Select fields to keep
            select(all_of(grouping_variables), "sii", "rii", "intercept")

          # join on dataset with SII/ RII confidence limits
          # Get CIs from first round of reps
          # Unnest confidence limits in a data frame for joining

          sim_CI_rep1 <- sim_CI %>%
            select(!c("data")) %>%
            tidyr::unnest("CI_params") |>
            slice_head(n = 1)

          if (length(grouping_variables) > 0) {
            # (grouped dataset)
            popsSII_model <- popsSII_model %>%
              left_join(sim_CI_rep1, by = grouping_variables)
          } else {
            # ungrouped dataset
            popsSII_model <- popsSII_model %>%
              cbind(sim_CI_rep1)
          }

          # Add reliability stats

          if (isTRUE(reliability_stat)) {

            sim_CI <- rename(sim_CI, "CI_calcs" = "CI_params")

            reliabity_stats <- calc_reliability(
              CI_data = sim_CI,
              confidence = confidence,
              rii = rii
            )

            if (length(grouping_variables) > 0) {
              # (grouped dataset)
              popsSII_model <- popsSII_model %>%
                left_join(reliabity_stats, by = grouping_variables)
            } else {
              # ungrouped dataset
              popsSII_model <- popsSII_model %>%
                cbind(reliabity_stats)
            }

          }

        } else {

          popsSII_model <- popsSII_model %>%
            mutate(sii = .data$b_sqrt_a,
                   intercept = .data$sqrt_a) %>%
            # Select fields to keep
            select(all_of(grouping_variables), "sii", "intercept")

          #Do calculations that can be done outside of loop as they don't need the CI fields
          if (value_type == 1) {#anti-log needed

            popsSII_model <- popsSII_model %>%
              mutate(xequals1 = .data$intercept + .data$sii,
                     xequalshalf = (.data$intercept + .data$xequals1) / 2,
                     antilogintercept = exp(.data$intercept),
                     antilogxequals1 = exp(.data$xequals1),
                     multiplier = .env$multiplier,
                     sii = (.data$antilogxequals1 - .data$antilogintercept) * .data$multiplier,
                     rii = if_else(
                       .data$multiplier < 0,
                       1 / (.data$antilogxequals1 / .data$antilogintercept),
                       .data$antilogxequals1 / .data$antilogintercept
                     ),
                     intercept = .data$antilogintercept * abs(.data$multiplier))

          } else if (value_type == 2) {#anti-logit needed

            popsSII_model <- popsSII_model %>%
              mutate(xequals1 = .data$intercept + .data$sii,
                     xequalshalf = (.data$intercept + .data$xequals1) / 2,
                     antilogintercept = exp(.data$intercept) / (1 + exp(.data$intercept)),
                     antilogxequals1 = exp(.data$xequals1) / (1 + exp(.data$xequals1)),
                     multiplier = .env$multiplier,
                     sii = (.data$antilogxequals1 - .data$antilogintercept) * .data$multiplier,
                     rii = if_else(
                       multiplier < 0,
                       1 / (.data$antilogxequals1 / .data$antilogintercept),
                       .data$antilogxequals1 / .data$antilogintercept
                     ),
                     intercept = .data$antilogintercept * abs(.data$multiplier))
          }


          popsSII_model_CIs <- popsSII_model |>
            select(all_of(grouping_variables), "xequalshalf")

          popsSII_model <- popsSII_model |>
            select(all_of(grouping_variables), "sii", "rii", "intercept")

          # join on dataset with confidence limits
          if (length(grouping_variables) > 0) {
            # (grouped dataset)
            popsSII_model_CIs <- popsSII_model_CIs %>%
              left_join(sim_CI, by = grouping_variables)
          } else {
            # ungrouped dataset
            popsSII_model_CIs <- popsSII_model_CIs %>%
              cbind(sim_CI)
          }

          # Calculate SII and RII for each rep

          popsSII_model_CIs <- popsSII_model_CIs |>
            mutate(
              CI_calcs = purrr::map2(.data$CI_params, .data$xequalshalf, function(data, xequalshalf) {

                map(confidence, function(conf) {

                  conf_formatted <-
                    gsub("\\.", "_", formatC(conf * 100, format = "f", digits = 1))

                  selected_data <- data %>%
                    select(contains(conf_formatted)) |>
                    select(contains("sii")) |>
                    rename(
                      "sii_lower" = contains("sii_lower"),
                      "sii_upper" = contains("sii_upper")
                    )

                  if (value_type == 1) {

                    SII_calculations <- selected_data %>%
                      mutate(interceptlcl = xequalshalf - (.data$sii_lower / 2),
                             interceptucl = xequalshalf - (.data$sii_upper / 2),
                             xequals1lcl = xequalshalf + (.data$sii_lower / 2),
                             xequals1ucl = xequalshalf + (.data$sii_upper / 2),
                             multiplier = .env$multiplier,
                             sii_lower = if_else(.data$multiplier < 1, (exp(.data$xequals1ucl) - exp(.data$interceptucl)) * .data$multiplier,
                                                 (exp(.data$xequals1lcl) - exp(.data$interceptlcl)) * .data$multiplier),
                             sii_upper = if_else(.data$multiplier < 1, (exp(.data$xequals1lcl) - exp(.data$interceptlcl)) * .data$multiplier,
                                                 (exp(.data$xequals1ucl) - exp(.data$interceptucl)) * .data$multiplier)
                      )

                    if (isTRUE(rii)) {
                      SII_calculations <- SII_calculations |>
                        mutate(
                          rii_lower = if_else(
                            .data$multiplier < 1,
                            1 / (exp(.data$xequals1ucl) / exp(.data$interceptucl)
                            ),
                            exp(.data$xequals1lcl) / exp(.data$interceptlcl)),
                          rii_upper = if_else(
                            .data$multiplier < 1,
                            1 / (exp(.data$xequals1lcl) / exp(.data$interceptlcl)),
                            exp(.data$xequals1ucl) / exp(.data$interceptucl)
                          )
                        )
                    }

                  } else if (value_type == 2) {

                    SII_calculations <- selected_data %>%
                      mutate(interceptlcl = xequalshalf - (.data$sii_lower / 2),
                             interceptucl = xequalshalf - (.data$sii_upper / 2),
                             xequals1lcl = xequalshalf + (.data$sii_lower / 2),
                             xequals1ucl = xequalshalf + (.data$sii_upper / 2),
                             multiplier = .env$multiplier,
                             sii_lower = if_else(
                               .data$multiplier < 0,
                               ((exp(.data$xequals1ucl) / (1 + exp(.data$xequals1ucl))) - (exp(.data$interceptucl) / (1 + exp(.data$interceptucl)))) * .data$multiplier,
                               ((exp(.data$xequals1lcl) / (1 + exp(.data$xequals1lcl))) - (exp(.data$interceptlcl) / (1 + exp(.data$interceptlcl)))) * .data$multiplier
                             ),
                             sii_upper = if_else(
                               .data$multiplier < 0,
                               ((exp(.data$xequals1lcl) / (1 + exp(.data$xequals1lcl))) - (exp(.data$interceptlcl) / (1 + exp(.data$interceptlcl)))) * .data$multiplier,
                               ((exp(.data$xequals1ucl) / (1 + exp(.data$xequals1ucl))) - (exp(.data$interceptucl)/(1 + exp(.data$interceptucl)))) * .data$multiplier
                             )
                      )

                    if (isTRUE(rii)) {

                      SII_calculations <- SII_calculations %>%
                        mutate(
                          rii_lower = if_else(
                            multiplier < 0,
                            1 / ((exp(.data$xequals1ucl) / (1 + exp(.data$xequals1ucl))) / (exp(.data$interceptucl) / (1 + exp(.data$interceptucl)))),
                            ((exp(.data$xequals1lcl)/(1 + exp(.data$xequals1lcl))) / (exp(.data$interceptlcl)/(1 + exp(.data$interceptlcl))))),
                          rii_upper = if_else(
                            .data$multiplier < 0,
                            1 / ((exp(.data$xequals1lcl) / (1 + exp(.data$xequals1lcl))) / (exp(.data$interceptlcl)/(1 + exp(.data$interceptlcl)))),
                            ((exp(.data$xequals1ucl) / (1 + exp(.data$xequals1ucl))) / (exp(.data$interceptucl)/(1 + exp(.data$interceptucl))))
                          )
                        )
                    }

                  }

                  SII_calculations <- SII_calculations |>
                    select(any_of(contains(c("sii_lower", "sii_upper",
                                             "rii_lower", "rii_upper")))) |>
                    rename_with(.fn = \(x) paste0(x, conf_formatted))

                }
                ) |>
                  bind_cols()
              }
              )
            ) |>
            select(all_of(grouping_variables), "CI_calcs")

          # Add CIs to model
          # join on dataset with SII/ RII confidence limits
          # Get CIs from first round of reps
          # Unnest confidence limits in a data frame for joining

          CI_rep1 <- popsSII_model_CIs %>%
            select(all_of(grouping_variables), "CI_calcs") %>%
            tidyr::unnest("CI_calcs") |>
            slice_head(n = 1)

          if (length(grouping_variables) > 0) {
            # (grouped dataset)
            popsSII_model <- popsSII_model %>%
              left_join(CI_rep1, by = grouping_variables)
          } else {
            # ungrouped dataset
            popsSII_model <- popsSII_model %>%
              cbind(CI_rep1)
          }

          # Add reliability stats
          if (isTRUE(reliability_stat)) {

            reliabity_stats <- calc_reliability(
              CI_data = popsSII_model_CIs,
              confidence = confidence,
              rii = rii
            )

            if (length(grouping_variables) > 0) {
              # (grouped dataset)
              popsSII_model <- popsSII_model %>%
                left_join(reliabity_stats, by = grouping_variables)
            } else {
              # ungrouped dataset
              popsSII_model <- popsSII_model %>%
                cbind(reliabity_stats)
            }

          }

        }

        # Part 3 - Choose and format output fields --------------------------------

        # Remove reliability stat columns (if not requested by user)
        if (reliability_stat == FALSE) {
          popsSII_model <- popsSII_model %>%
            select(-contains("mad"))
        }

        # Remove RII columns (if not requested by user)
        if(rii == FALSE) {
          popsSII_model <- popsSII_model %>%
            select(!contains("rii"))
        }

        # Remove intercept columns (if not requested by user)
        if(intercept == FALSE) {
          popsSII_model <- popsSII_model %>%
            select(!"intercept")
        }

        # Move intercept to last column of dataframe
        if(intercept == TRUE) {
          popsSII_model <- popsSII_model %>%
            select(!"intercept", "intercept")
        }

        # Add metadata columns to output dataset (if requested by user)
        if (type == "full") {

          popsSII_model  <- popsSII_model %>%

            mutate(indicator_type = if_else(value_type == 0, "normal",
                                            if_else(value_type == 1,
                                                  "rate", "proportion")),
                   multiplier = multiplier,
                   transform = if_else(transform == TRUE & value_type == 1, "log",
                                       if_else(transform == TRUE & value_type == 2, "logit",
                                             "none")),
                   CI_confidence = paste0(confidence * 100, "%",
                                          collapse = ", "),
                   CI_method = paste0("simulation ", repetitions, " reps"))

        }

        # return output dataset
        return(popsSII_model)

}
