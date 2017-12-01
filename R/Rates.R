# Need to add preamble to generate help files

# create phe_rate function to calculate a rate from numerator and denominator
# with optional multiplier and CI inputs.  Byars CI method



# see https://cran.r-project.org/web/packages/exactci/exactci.pdf
# see https://cran.r-project.org/web/packages/ratesci/index.html
# see https://cran.r-project.org/web/packages/epitools/epitools.pdf
# see https://www.rdocumentation.org/packages/epitools/versions/0.5-7/topics/ageadjust.direct
# ems package has SMR function shere can specify Byars Ci method


#______________________________________________________________________________________________________

byars_lower <- function(x) {
  byars_lower <- x*(1-1/(9*x)-qnorm(1-0.05/2)/(3*sqrt(x)))^3
  return(byars_lower)
}

byars_upper <- function(x) {
  byars_upper <- (x+1)*(1-1/(9*(x+1))+qnorm(1-0.05/2)/(3*sqrt(x+1)))^3
  return(byars_upper)
  }

phe_rate <- function(x, n, conf.level = 0.95, multiplier = 100000) {

  # add stops

  rate <- x/n*multiplier
  lowercl<-byars_lower(x)/n*multiplier
  uppercl<-byars_upper(x)/n*multiplier

  phe_rate <- data.frame("Byars", x, n, rate, lowercl, uppercl)
  names(phe_rate) <- c("method","numerator","denominator","rate",paste("lower",conf.level*100,"cl",sep=""),paste("upper",conf.level*100,"cl",sep=""))
  return(phe_rate)
}
