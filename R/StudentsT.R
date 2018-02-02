# -------------------------------------------------------------------------------------------------
#' zscore
#'
#' Calculates z scores (percentile values) from the Standard Normal distribution
#'
#' @inheritParams phe_dsr
#'
#' @return Returns a z score for the required percentile value from the Standard Normal distirbution
#'
#' @examples
#' zscore(0.95)
#' zscore(99.8)
#'
#' @export
#'
#' @family PHEstatmethods package functions
# -------------------------------------------------------------------------------------------------

# create zscore function
zscore <- function(confidence = 0.95) {

  # validate arguments
  if ((confidence<0.9)|(confidence >1 & confidence <90)|(confidence > 100)) {
    stop("confidence level must be between 90 and 100 or between 0.9 and 1")
  }

  # scale confidence level
  if (confidence >= 90) {
    confidence <- confidence/100
  }

  # calculate zscore
  zscore <- qnorm(confidence+(1-confidence)/2)

  return(zscore)
}
