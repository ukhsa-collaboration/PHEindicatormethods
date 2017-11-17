# Need to add preamble to generate help files

# create phe_proportion function to execute binom.confint with method fixed to wilson
phe_proportion <- function(x, n, conf.level=0.95, max=1) {
    if (x < 0) {
              stop("numerator must be greater than or equal to zero")
      } else if (n <= 0) {
              stop("denominator must be greater than zero")
      } else if (x > n) {
              stop("numerator must be less than or equal to denominator for a proportion statistic")
      } else if (!(max %in% c(1,100))) {
              stop("function phe_proportion can only output a proportion (max=1) or a percentage (max=100)")
      }
    phe_proportion <- binom.confint(x, n, conf.level, methods="wilson") %>%
      select(x,n,mean,lower,upper) * c(1,1,max,max,max) %>%
      mutate(method="wilson")
    return(phe_proportion)
}
