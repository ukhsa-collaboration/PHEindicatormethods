# Need to add preamble to generate help files

# create phe_proportion function to execute binom.confint with method fixed to wilson
phe_proportion <- function(x, n, conf.level=0.95, percentage=FALSE) {
    if (x < 0) {
              stop("numerator must be greater than or equal to zero")
      } else if (n <= 0) {
              stop("denominator must be greater than zero")
      } else if (x > n) {
              stop("numerator must be less than or equal to denominator for a proportion statistic")
      } else if ((conf.level<0.9)|(conf.level >1 & conf.level <90)|(conf.level > 100)) {
              stop("confidence interval must be between 90 and 99.99")
      }


# scale confidence interval
if (conf.level >= 90) {
  conf.level <- conf.level/100
}

# set multiplier
multiplier <- 1
if (percentage == TRUE) {
  multiplier <- 100
}

# calculate proportion and CIs
phe_proportion <- data.frame(binom.confint(x, n, conf.level, methods="wilson")) %>%
        mutate(mean = mean * multiplier) %>%
        mutate(lower = lower * multiplier) %>%
        mutate(upper = upper * multiplier)

# set column names to be output
names(phe_proportion) <- c("method","numerator","denominator","proportion",paste("lower",conf.level*100,"cl",sep=""),paste("upper",conf.level*100,"cl",sep=""))

return(phe_proportion)
}
