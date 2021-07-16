# -------------------------------------------------------------------------------------------------
#' Convert NAs to zeros using na.zero
#'
#' converts NAs to zeros
#'
#' @param y input vector
#'
# -------------------------------------------------------------------------------------------------
na.zero <- function (y) {
    y[is.na(y)] <- 0
    return(y)
}



# -------------------------------------------------------------------------------------------------
#' byars_lower
#'
#' Calculates the lower confidence limits for observed numbers of events using Byar's method [1].
#'
#' @param x the observed numbers of events; numeric vector; no default
#'
#' @inheritParams phe_dsr
#'
#' @return Returns lower confidence limits for observed numbers of events using Byar's method [1]
#'
#' @section Notes: This is an internal package function that is appropriately called by exported
#'  'phe_' prefixed functions within the PHEindicatormethods package.  \cr \cr
#'  \code{byars_lower} and \code{\link{byars_upper}} together return symmetric confidence
#'  intervals around counts, therefore
#'  for a specified confidence level, \eqn{\alpha}, the probability that, by chance, the lower limit returned
#'  will be above the true underlying value, is \eqn{\alpha}/2.
#'  If the confidence level is very close to 1 or the number of events is very small
#'  Byar's method is inaccurate and may return a negative number - in these cases an error is returned.
#'
#' @references
#' [1] Breslow NE, Day NE. Statistical methods in cancer research,
#'  volume II: The design and analysis of cohort studies. Lyon: International
#'  Agency for Research on Cancer, World Health Organisation; 1987.
#'
#' -------------------------------------------------------------------------------------------------

# create function to calculate Byar's lower CI limit
byars_lower <- function(x, confidence = 0.95) {

    # validate arguments
    if (any(x < 0, na.rm=TRUE)) {
        stop("observed events must all be greater than or equal to zero")
    } else if ((confidence<0.9)|(confidence >1 & confidence <90)|(confidence > 100)) {
        stop("confidence level must be between 90 and 100 or between 0.9 and 1")
    }

    # scale confidence level
    if (confidence >= 90) {
        confidence <- confidence/100
    }

    # populate z
    z <- qnorm(confidence + (1-confidence)/2)

    # calculate
    byars_lower <- x*(1-1/(9*x)-z/(3*sqrt(x)))^3

    # set negative values to NA
    byars_lower[byars_lower < 0]  <- NA

    return(byars_lower)

}


# -------------------------------------------------------------------------------------------------
#' byars_upper
#'
#' Calculates the upper confidence limits for observed numbers of events using Byar's method [1].
#'
#' @param x the observed numbers of events; numeric vector; no default
#'
#' @inheritParams phe_dsr
#'
#' @return Returns upper confidence limits for observed numbers of events using Byar's method [1]
#'
#' @section Notes: This is an internal package function that is appropriately called by exported
#'  'phe_' prefixed functions within the PHEindicatormethods package.  \cr \cr
#'  \code{\link{byars_lower}} and \code{byars_upper} together return symmetric confidence
#'  intervals around counts, therefore
#'  for a specified confidence level, \eqn{\alpha}, the probability that, by chance, the upper limit returned
#'  will be below the true underlying value, is \eqn{\alpha}/2.
#'
#' @references
#' [1] Breslow NE, Day NE. Statistical methods in cancer research,
#'  volume II: The design and analysis of cohort studies. Lyon: International
#'  Agency for Research on Cancer, World Health Organisation; 1987.
#'
#' -------------------------------------------------------------------------------------------------

# create function to calculate Byar's upper CI limit
byars_upper <- function(x, confidence = 0.95) {

    # validate arguments
    if (any(x < 0, na.rm=TRUE)) {
        stop("observed events must all be greater than or equal to zero")
    } else if ((confidence<0.9)|(confidence >1 & confidence <90)|(confidence > 100)) {
        stop("confidence level must be between 90 and 100 or between 0.9 and 1")
    }

    # scale confidence level
    if (confidence >= 90) {
        confidence <- confidence/100
    }

    # populate z
    z <- qnorm(confidence + (1-confidence)/2)

    byars_upper <- (x+1)*(1-1/(9*(x+1))+z/(3*sqrt(x+1)))^3
    return(byars_upper)
}


# -------------------------------------------------------------------------------------------------
#' wilson_lower
#'
#' Calculates lower confidence limits for observed numbers of events using the Wilson Score method [1,2].
#'
#' @param x the observed numbers of cases in the samples meeting the required condition; numeric vector; no default
#' @param n the numbers of cases in the samples; numeric vector; no default
#'
#' @inheritParams phe_dsr
#'
#' @return Returns lower confidence limits for observed numbers of events using the Wilson Score method [1,2]
#'
#' @section Notes: This is an internal package function that is appropriately called by exported
#'  'phe_' prefixed functions within the PHEindicatormethods package.  \cr \cr
#'  \code{wilson_lower} and \code{\link{wilson_upper}} together return symmetric confidence
#'  intervals, therefore for a specified confidence level, \eqn{\alpha}, the probability that, by chance, the lower limit returned
#'  will be above the true underlying value, is \eqn{\alpha}/2.#'
#'
#' @references
#' [1] Wilson EB. Probable inference, the law of succession, and statistical
#'  inference. J Am Stat Assoc; 1927; 22. Pg 209 to 212. \cr
#' [2] Newcombe RG, Altman DG. Proportions and their differences. In Altman
#'  DG et al. (eds). Statistics with confidence (2nd edn). London: BMJ Books;
#'  2000. Pg 46 to 48.
#'
#' ------------------------------------------------------------------------------------------------

# create function to calculate Wilson's lower CI limit
wilson_lower <- function(x, n, confidence = 0.95) {

    # validate arguments
    if (any(x < 0, na.rm=TRUE)) {
        stop("observed events must all be greater than or equal to zero")
    } else if (any(n < 0, na.rm=TRUE)) {
        stop("sample sizes must all be greater than zero")
    } else if (any(x > n, na.rm=TRUE)) {
        stop("numerators must be less than or equal to denominator for a Wilson score to be calculated")
    } else if ((confidence<0.9)|(confidence >1 & confidence <90)|(confidence > 100)) {
        stop("confidence level must be between 90 and 100 or between 0.9 and 1")
    }

    # scale confidence level
    if (confidence >= 90) {
        confidence <- confidence/100
    }

    if (confidence == 1) {
        wilson_lower <- 0
    } else {
        # set z
        z <- qnorm(confidence+(1-confidence)/2)
        # calculate
        wilson_lower <- (2*x+z^2-z*sqrt(z^2+4*x*(1-(x/n))))/2/(n+z^2)
    }

  return(wilson_lower)
}



# -------------------------------------------------------------------------------------------------
#' wilson_upper
#'
#' Calculates upper confidence limits for observed numbers of events using the Wilson Score method [1,2].
#'
#' @param x the observed numbers of cases in the samples meeting the required condition; numeric vector; no default
#' @param n the numbers of cases in the samples; numeric vector; no default
#'
#' @inheritParams phe_dsr
#'
#' @return Returns upper confidence limits for observed numbers of events using the Wilson Score method [1,2]
#'
#' @section Notes: This is an internal package function that is appropriately called by exported
#'  'phe_' prefixed functions within the PHEindicatormethods package.  \cr \cr
#'  \code{\link{wilson_lower}} and \code{wilson_upper} together return symmetric confidence
#'  intervals, therefore for a specified confidence level, \eqn{\alpha}, the probability that, by chance, the upper limit returned
#'  will be below the true underlying value, is \eqn{\alpha}/2.#'
#'
#' @references
#' [1] Wilson EB. Probable inference, the law of succession, and statistical
#'  inference. J Am Stat Assoc; 1927; 22. Pg 209 to 212. \cr
#' [2] Newcombe RG, Altman DG. Proportions and their differences. In Altman
#'  DG et al. (eds). Statistics with confidence (2nd edn). London: BMJ Books;
#'  2000. Pg 46 to 48.
#'
#' ------------------------------------------------------------------------------------------------

# create function to calculate Wilson's lower CI limit
wilson_upper <- function(x, n, confidence = 0.95) {

    # validate arguments
    if (any(x < 0, na.rm=TRUE)) {
        stop("observed events must all be greater than or equal to zero")
    } else if (any(n < 0, na.rm=TRUE)) {
        stop("sample sizes must all be greater than zero")
    } else if (any(x > n, na.rm=TRUE)) {
        stop("numerators must be less than or equal to denominator for a Wilson score to be calculated")
    } else if ((confidence<0.9)|(confidence >1 & confidence <90)|(confidence > 100)) {
        stop("confidence level must be between 90 and 100 or between 0.9 and 1")
    }

    # scale confidence level
    if (confidence >= 90) {
        confidence <- confidence/100
    }

    if (confidence == 1) {
        wilson_upper <- 1
    } else {
        # set z
        z <- qnorm(confidence+(1-confidence)/2)
        # calculate
        wilson_upper <- (2*x+z^2+z*sqrt(z^2+4*x*(1-(x/n))))/2/(n+z^2)
    }

    return(wilson_upper)
}

# -------------------------------------------------------------------------------------------------
#' FindXValues
#'
#' Calculates mid-points of cumulative population for each quantile
#'
#' @param xvals field name in input dataset that contains the quantile
#'        populations; unquoted string; no default
#' @param no_quantiles number of quantiles supplied in dataset for SII;
#'        integer; no default
# -------------------------------------------------------------------------------------------------

FindXValues <- function(xvals, no_quantiles){

  # Create blank table with same number of rows as quantiles
  df <- data.frame(prop = numeric(no_quantiles),
                   cumprop = numeric(no_quantiles),
                   output = numeric(no_quantiles))

  # Calculates each group's population as proportion of total population
  df$prop <-xvals/sum(xvals, na.rm=TRUE)

  # Calculate cumulative proportion for subsequent groups
  df$cumprop = cumsum(df$prop)

  # lag calculates difference between the two proportions
  # "output" is the calculated mid-point of each proportion segment
  df$output = ifelse(is.na(lag(df$cumprop,1)), #(the lag function will produce NA for first item)
                     df$prop/2,
                     df$prop/2 + lag(df$cumprop,1))

  # Return output field
  FindXValues <- df$output

}

# -------------------------------------------------------------------------------------------------
#' SimulationFunc
#'
#' Function to simulate SII range through random sampling of the indicator value
#' for each quantile, based on the associated mean and standard error
#'
#' @return returns lower and upper SII confidence limits according to user
#' specified confidence
#'
#' @param data data.frame containing the data to calculate SII confidence limits
#'        from; unquoted string; no default
#' @param value field name within data that contains the indicator value; unquoted
#'        string; no default
#' @param value_type indicates the indicator type (1 = rate, 2 = proportion, 0 = other);
#'        integer; default 0
#' @param se field name within data that contains the standard error of the indicator
#'        value; unquoted string; no default
#' @param repeats number of random samples to perform to return confidence interval of SII;
#'        numeric; default 100,000
#' @param confidence confidence level used to calculate the lower and upper confidence limits of SII;
#'        numeric between 0.5 and 0.9999 or 50 and 99.99; default 0.95
#' @param multiplier factor to multiply the SII and SII confidence limits by (e.g. set to 100 to return
#'        prevalences on a percentage scale between 0 and 100). If the multiplier is negative, the
#'        inverse of the RII is taken to account for the change in polarity; numeric; default 1;
#' @param sqrt_a field name within dataset containing square root of a values;
#'        unquoted string; no default
#' @param b_sqrt_a field name within dataset containing square root of a values multiplied
#'        by b values;unquoted string; no default
#' @param rii option to return the Relative Index of Inequality (RII) with associated confidence limits
#'        as well as the SII; logical; default FALSE
#' @param reliability_stat option to carry out the SII confidence interval simulation 10 times instead
#'        of once and return the Mean Average Difference between the first and subsequent samples (as a
#'        measure of the amount of variation); logical; default FALSE
#'
# -------------------------------------------------------------------------------------------------

SimulationFunc <- function(data,
                           value,
                           value_type = 0,
                           se,
                           repeats = 100000,
                           confidence = 0.95,
                           multiplier = 1,
                           sqrt_a,
                           b_sqrt_a,
                           rii = FALSE,
                           reliability_stat = FALSE) {

  # Use NSE on input fields - apply quotes
  value = enquo(value)
  se = enquo(se)
  sqrt_a = enquo(sqrt_a)
  b_sqrt_a = enquo(b_sqrt_a)

  # find critical upper value at given confidence
  confidence_value <- confidence + ((1 - confidence) / 2)

  # Set 10x no. of reps if reliability stats requested
  no_reps <- ifelse(reliability_stat == TRUE, 10*repeats, repeats)

  # Take random samples from a normal distribution with mean as the indicator value
  # sd. as the associated standard error. Store results in a
  # (no. of quantiles)x(no. of repeats) dimensional matrix.
  yvals <- matrix(stats::rnorm(no_reps*length(pull(data, !!value)), pull(data, !!value), pull(data, !!se)), ncol = no_reps)

  # Retransform y values for rates (1) and proportions (2)
  if (value_type == 1) {
    yvals_transformed <- exp(yvals)
  } else if (value_type == 2){
    yvals_transformed <- exp(yvals)/(1+exp(yvals))
  } else {
    yvals_transformed <- yvals
  }

  # Combine explanatory variables sqrta and bsqrta into a matrix
  # (This is the transpose of matrix X in the regression formula)
  m <- as.matrix(rbind(pull(data, !!sqrt_a), pull(data, !!b_sqrt_a)))

  # Calculate inverse of (m*transpose (m))*m to use in calculation
  # of least squares estimate of regression parameters
  # Ref: https://onlinecourses.science.psu.edu/stat501/node/38
  invm_m <- solve((m)%*%t(m))%*%m

  # Multiply transformed yvals matrix element-wise by sqrta - this weights the sampled
  # yvals by a measure of population
  final_yvals <- yvals_transformed*pull(data, !!sqrt_a)

  # Prepare empty numeric vector to hold parameter results
  sloperesults <- numeric(no_reps)

  # Define function to matrix multiply invm_m by a vector
  matrix_mult <- function(x) {invm_m %*% x}

  # Carry out iterations - matrix multiply invm_m by each column of
  # population-weighted yvals in turn
  temp <- apply(final_yvals, 2, matrix_mult)

  # Extract the b_sqrt_a and sqrt_a parameters
  params_bsqrta <- temp[2,]
  params_sqrta <- temp[1,]

  # RII only calculations
  if (rii == TRUE) {

      # Calculate the RII
      RII_results <- (params_bsqrta + params_sqrta)/params_sqrta

      # Split simulated SII/RIIs into 10 samples if reliability stats requested
      if (reliability_stat == TRUE) {
        SII_results <- matrix(params_bsqrta, ncol = 10)
        RII_results <- matrix(RII_results, ncol = 10)
      } else {
        SII_results <- matrix(params_bsqrta, ncol = 1)
        RII_results <- matrix(RII_results, ncol = 1)
      }

      # Apply multiplicative factor to RII
      if(multiplier < 0) {
        RII_results <- 1/RII_results
      }

      # Order simulated RIIs from lowest to highest
      sortresults_RII <- apply(RII_results, 2, sort, decreasing = FALSE)

  } else {

    # Split simulated SII/RIIs into 10 samples if reliability stats requested
    if (reliability_stat == TRUE) {
      SII_results <- matrix(params_bsqrta, ncol = 10)
    } else {
      SII_results <- matrix(params_bsqrta, ncol = 1)
    }
  }

  # Apply multiplicative factor to SII
  SII_results <- multiplier * SII_results

  # Order simulated SIIs from lowest to highest
  sortresults_SII <- apply(SII_results, 2, sort, decreasing = FALSE)

  # Extract specified percentiles (2.5 and 97.5 for 95% confidence) and return
  # as confidence limits

  # position of lower percentile
  pos_lower <- round(repeats*(1-confidence_value), digits=0)
  # position of upper percentile
  pos_upper <- round(repeats*confidence_value, digits=0)

  # Combine position indexes for SII CLs
  pos <- rbind(pos_lower, pos_upper)

  # Extract SII confidence limits from critical percentiles of first column of samples
  SII_lower_cls <- sortresults_SII[pos_lower, 1]
  SII_upper_cls <- sortresults_SII[pos_upper, 1]

  # Define column names (adding in confidence level)
  names(SII_lower_cls) <- paste0("sii_lower",
                             gsub("\\.", "_", formatC(confidence * 100, format = "f", digits = 1)),
                             "cl")
  names(SII_upper_cls) <- paste0("sii_upper",
                             gsub("\\.", "_", formatC(confidence * 100, format = "f", digits = 1)),
                             "cl")

  # Combine lower and upper SII CLs
  SII_cls <- t(c(SII_lower_cls, SII_upper_cls))

  # CASE 1 - Calculate RII CLs if requested
  if(rii == TRUE) {

    # Extract RII confidence limits from critical percentiles of first column of samples
    RII_lower_cls <- sortresults_RII[pos_lower, 1]
    RII_upper_cls <- sortresults_RII[pos_upper, 1]

    # Define column names (adding in confidence level)
    names(RII_lower_cls) <- paste0("rii_lower",
                                   gsub("\\.", "_", formatC(confidence * 100, format = "f", digits = 1)),
                                   "cl")
    names(RII_upper_cls) <- paste0("rii_upper",
                                   gsub("\\.", "_", formatC(confidence * 100, format = "f", digits = 1)),
                                   "cl")

    # Combine lower and upper RII CLs
    RII_cls <- t(c(RII_lower_cls, RII_upper_cls))

    if (reliability_stat == TRUE) {

      # Calculate variability by taking the absolute difference between each of the lower/upper
      # limits in the additional 9 sample sets and the initial lower/upper limits
      SII_diffs <- apply(pos, 2, function(x) { # run function over each column of "pos" (i.e. for each confidence)

        diffs_sample_original <- t(apply(sortresults_SII[c(x[1], x[2]),], 1, function(y) abs(y - y[1])))

        # Calculate mean absolute difference over all 18 differences
        sii_mad <- mean(diffs_sample_original[, 2:10])
      })

      RII_diffs <- apply(pos, 2, function(x) { # run function over each column of "pos" (i.e. for each confidence)

        diffs_sample_original <- t(apply(sortresults_RII[c(x[1], x[2]),], 1, function(y) abs(y - y[1])))

        # Calculate mean absolute difference over all 18 differences
        rii_mad <- mean(diffs_sample_original[, 2:10])
      })

      # Define column names (adding in confidence level)
      names(SII_diffs) <- paste0("sii_mad",
                                 gsub("\\.", "_", formatC(confidence * 100, format = "f", digits = 1)))
      names(RII_diffs) <- paste0("rii_mad",
                                 gsub("\\.", "_", formatC(confidence * 100, format = "f", digits = 1)))

      # Return SII confidence limits from first of the 10 samples, plus the
      # reliability measures
      results <- cbind(SII_cls, RII_cls, t(SII_diffs), t(RII_diffs))


    } else {
      # Return SII and RII confidence limits from single sample taken
      results <- cbind(SII_cls, RII_cls)
    }

  # CASE 2 - Return SII stats only
  } else {

    if (reliability_stat == TRUE) {

      # Calculate variability by taking the absolute difference between each of the lower/upper
      # limits in the additional 9 sample sets and the initial lower/upper limits
      SII_diffs <- apply(pos, 2, function(x) { # run function over each column of "pos" (i.e. for each confidence)

                       diffs_sample_original <- t(apply(sortresults_SII[c(x[1], x[2]),], 1, function(y) abs(y - y[1])))

                       # Calculate mean absolute difference over all 18 differences
                       sii_MAD <- mean(diffs_sample_original[, 2:10])
                    })

      # Define column names (adding in confidence level)
      names(SII_diffs) <- paste0("sii_mad",
                                     gsub("\\.", "_", formatC(confidence * 100, format = "f", digits = 1)))

      # Return SII confidence limits from first of the 10 samples, plus the
      # reliability measures
      results <- cbind(SII_cls, t(SII_diffs))

    } else {
      # Return SII confidence limits only from single sample taken
      results <- SII_cls
    }
  }

  return(data.frame(results))

}



poisson_cis <- function(z, x_a, x_b) {
  # this is in the Excel macro code, but none of the following can occur based
  # on Funnels.R
  # if (any(z < 0, x_a < 0, x_b < 0, x_b < x_a, x_a %% 1 > 0, x_b %% 1 > 0))
  #   return(NA)

  q <- 1
  tot <- 0
  s <- 0
  k <- 0

  while (any(k <= z, q > tot * 1e-10)) {
    tot <- tot + q
    if (k >= x_a & k <= x_b) s <- s + q
    if (tot > 1e30) {
      s <- s / 1e30
      tot <- tot / 1e30
      q <- q / 1e30
    }

    k <- k + 1
    q <- q * z / k

  }
  return(s / tot)
}

poisson_funnel <- function(obs, p, side) {
  # this is in the Excel macro code, but none of the following can occur based
  # on Funnels.R
  # if (any(obs < 0, p < 0, p > 1, obs %% 1 != 0)) return(NA)

  v <- 0.5
  dv <- 0.5

  if (side == "low") {

    # this is in the Excel macro code, but obs can't be 0 based on Funnels.R
    # if (obs == 0) return(0)

    while (dv > 1e-7) {
      dv <- dv / 2

      if (poisson_cis((1 + obs) * v / (1 - v),
                      obs,
                      10000000000) > p) {
        v <- v - dv
      } else {
        v <- v + dv
      }
    }
  } else if (side == "high") {

    while (dv > 1e-7) {
      dv <- dv / 2
      if (poisson_cis((1 + obs) * v / (1 - v),
                      0,
                      obs) < p) {
        v <- v - dv
      } else {
        v <- v + dv
      }
    }

  }
  p <- (1 + obs) * v / (1 - v)
  return(p)
}


funnel_ratio_significance <- function(obs, expected, p, side) {
  if (obs == 0 & side == "low") {
    test_statistic <- 0
  } else if (obs < 10) {
    if (side == "low") {
      degree_freedom <- 2 * obs
      lower_tail_setting <- FALSE
    } else if (side == "high") {
      degree_freedom <- 2 * obs + 2
      lower_tail_setting <- TRUE
    }

    test_statistic <- qchisq(p = 0.5 + p / 2,
                             df = degree_freedom,
                             lower.tail = lower_tail_setting) / 2
  } else {
    if (side == "low") {
      obs_adjusted <- obs
      test_statistic <- obs_adjusted * (1 - 1 / (9 * obs_adjusted) -
                                          qnorm(0.5 + p / 2) / 3 / sqrt(obs_adjusted))^3
    } else if (side == "high") {
      obs_adjusted <- obs + 1
      test_statistic <- obs_adjusted * (1 - 1 / (9 * obs_adjusted) +
                                          qnorm(0.5 + p / 2) / 3 / sqrt(obs_adjusted))^3
    }
  }

  test_statistic <- test_statistic / expected
  return(test_statistic)
}
