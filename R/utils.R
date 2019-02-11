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
  confidence <- confidence + ((1 - confidence) / 2)

  # Set 10x no. of reps if reliability stats requested
  no_reps <- ifelse(reliability_stat == TRUE, 10*repeats, repeats)

  # Take random samples from a normal distribution with mean as the indicator value
  # sd. as the associated standard error. Store results in a
  # (no. of quantiles)x(no. of repeats) dimensional matrix.
  yvals <- matrix(rnorm(no_reps*length(pull(data, !!value)), pull(data, !!value), pull(data, !!se)), ncol = no_reps)

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

  # Order simulated SIIs from lowest to highest
  sortresults_SII <- apply(SII_results, 2, sort, decreasing = FALSE)

  # Extract specified percentiles (2.5 and 97.5 for 95% confidence) and return
  # as confidence limits

  # position of lower percentile
  pos_lower <- round(repeats*(1-confidence), digits=0)
  # position of upper percentile
  pos_upper <- round(repeats*confidence, digits=0)

  # Extract SII confidence limits from critical percentiles of first column of samples
  SII_initial <- sortresults_SII[c(pos_lower, pos_upper), 1]

  if(rii == TRUE) {

    # Extract SII confidence limits from critical percentiles of first column of samples
    RII_initial <- sortresults_RII[c(pos_lower, pos_upper), 1]

    if (reliability_stat == TRUE) {

      # Calculate variability by taking the absolute difference between each of the lower/upper
      # limits in the additional 9 sample sets and the initial lower/upper limits
      SII_diffs <- t(apply(sortresults_SII[c(pos_lower, pos_upper),], 1, function(x) abs(x - x[1])))
      RII_diffs <- t(apply(sortresults_RII[c(pos_lower, pos_upper),], 1, function(x) abs(x - x[1])))

      # Calculate mean absolute difference over all 18 differences
      sii_MAD <- mean(SII_diffs[, 2:10])
      rii_MAD <- mean(RII_diffs[, 2:10])

      # Return SII/RII confidence limits from first of the 10 samples, plus the
      # reliability measures
      return(data.frame(sii_lowercl = SII_initial[1],
                        sii_uppercl = SII_initial[2],
                        sii_MAD = signif(sii_MAD, 4),
                        rii_lowercl = RII_initial[1],
                        rii_uppercl = RII_initial[2],
                        rii_MAD = signif(rii_MAD, 4)))
    } else {
      # Return SII/RII confidence limits from single sample taken
      return(data.frame(sii_lowercl = SII_initial[1],
                        sii_uppercl = SII_initial[2],
                        sii_MAD = NA,
                        rii_lowercl = RII_initial[1],
                        rii_uppercl = RII_initial[2],
                        rii_MAD = NA))
    }

  } else {

    if (reliability_stat == TRUE) {

      # Calculate variability by taking the absolute difference between each of the lower/upper
      # limits in the additional 9 sample sets and the initial lower/upper limits
      SII_diffs <- t(apply(sortresults_SII[c(pos_lower, pos_upper),], 1, function(x) abs(x - x[1])))

      # Calculate mean absolute difference over all 18 differences
      sii_MAD <- mean(SII_diffs[, 2:10])

      # Return SII/RII confidence limits from first of the 10 samples, plus the
      # reliability measures
      return(data.frame(sii_lowercl = SII_initial[1],
                        sii_uppercl = SII_initial[2],
                        sii_MAD = signif(sii_MAD, 4),
                        rii_lowercl = NA,
                        rii_uppercl = NA,
                        rii_MAD = NA))
    } else {
      # Return SII/RII confidence limits from single sample taken
      return(data.frame(sii_lowercl = SII_initial[1],
                        sii_uppercl = SII_initial[2],
                        sii_MAD = NA,
                        rii_lowercl = NA,
                        rii_uppercl = NA,
                        rii_MAD = NA))
    }
  }

}
