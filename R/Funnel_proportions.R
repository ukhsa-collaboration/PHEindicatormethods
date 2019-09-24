# -------------------------------------------------------------------------------------------------
#' Calculate confidence intervals/control limits and levels of significance
#'
#' Calculates confidence intervals adopting consistent method as the excel APHO tools
#'
#' @param data a data.frame containing the data to calculate control limits for, pre-grouped if CLs required for
#'             group aggregates; unquoted string; no default
#' @param x field name from data containing the observed numbers of cases in the sample meeting the required condition
#'          (the numerator for the CLs); unquoted string; no default
#' @param n field name from data containing the population(s) in the sample (the denominator for the CLs);
#'          unquoted string; no default
#' @param multiplier the multiplier used to express the final values (eg 100 = percentage); numeric; default 1
#'
#' @inheritParams phe_dsr
#'
#' @return When type = "full", returns the original data.frame with the following appended:
#'         percentage, lower 0.025 limit, upper 0.025 limite, lower 0.001 limit, upper 0.001 limit and baseline average
#'
#' @import dplyr
#' @importFrom rlang sym quo_name :=
#'
#'
#' @examples
#'
#' # ungrouped data frame
#' df <- data.frame(area = rep(c("Area1","Area2","Area3","Area4"), each=3),
#'                  numerator = c(NA,82,9,48, 6500,8200,10000,10000,8,7,750,900),
#'                  denominator = rep(c(100,10000,10000,10000), each=3))
#'
#' phe_funnelcls_proportion(df, numerator, denominator)
#' phe_funnelcls_proportion(df, numerator, denominator, type="standard")
#'
#'
#' # grouped data frame
#' library(dplyr)
#' dfg <- df %>% group_by(area)
#' phe_funnelcls_proportion(dfg, numerator, denominator, multiplier=100)
#'
#'
#' @export
#'
#'
#' @family PHEindicatormethods package functions
# -------------------------------------------------------------------------------------------------

# create phe_proportion function
phe_funnelcls_proportion<-function(data,x,n){

library(dplyr)
# check required arguments present
    if (missing(data)|missing(x)|missing(n)) {
        stop("function phe_proportion requires at least 3 arguments: data, x, n")
        }


       # apply quotes
    x<-enquo(x)
    n<-enquo(n)


    #calculate the initialisation variables
    prop<-x/n
    prop_calc<-prop*100
    av<-sum(x)/sum(n)
    mind<-min(n)
    maxd<-max(n)
    actmind<- if(maxd>2*mind){0
    } else {floor(mind*0.95)}
    actmaxd<- ceiling(maxd)
    minp<-min(prop_calc)
    maxp<-max(prop_calc)
    actminp<- if(maxp>2*minp){0
    } else {floor(minp*0.95)}

    # First create a vector with the numbers 1 to 100 (as the plot will have 100 data points)

    #Create a blank matrix which will be populated with results obtained above

    t<-matrix(ncol=7,nrow=100)
    colnames(t)<-c("Row.number","Population","Lower2s0025limit","Upper2s0025limit","Lower3s0001limit",
                   "Upper3s0001limit","Baseline")

    t[1,1]=1

    t[1,2]=1
    t[1,3]= max(0,((av*(t[1,2]/qnorm(0.975)^2+1)-sqrt((-8*av*(t[1,2]/qnorm(0.975)^2+1))^2-64*(1/qnorm(0.975)^2+1/t[1,2])*av*(t[1,2]*(av*(t[1,2]/qnorm(0.975)^2+2)-1)+qnorm(0.975)^2*
                                                                                                                                 (av-1)))/8)/(1/qnorm(0.975)^2+1/t[1,2])/t[1,2])*100)
    t[1,4]= min(100,((av*(t[1,2]/qnorm(0.975)^2+1)+sqrt((-8*av*(t[1,2]/qnorm(0.975)^2+1))^2-64*(1/qnorm(0.975)^2+1/t[1,2])*av*(t[1,2]*(av*(t[1,2]/qnorm(0.975)^2+2)-1)+qnorm(0.975)^2*
                                                                                                                                   (av-1)))/8)/(1/qnorm(0.975)^2+1/t[1,2])/t[1,2])*100)

    t[1,5]= max(0,((av*(t[1,2]/qnorm(0.999)^2+1)-sqrt((-8*av*(t[1,2]/qnorm(0.999)^2+1))^2-64*(1/qnorm(0.999)^2+1/t[1,2])*av*(t[1,2]*(av*(t[1,2]/qnorm(0.999)^2+2)-1)+qnorm(0.999)^2*
                                                                                                                                 (av-1)))/8)/(1/qnorm(0.999)^2+1/t[1,2])/t[1,2])*100)

    t[1,6]= min(100,((av*(t[1,2]/qnorm(0.999)^2+1)+sqrt((-8*av*(t[1,2]/qnorm(0.999)^2+1))^2-64*(1/qnorm(0.999)^2+1/t[1,2])*av*(t[1,2]*(av*(t[1,2]/qnorm(0.999)^2+2)-1)+qnorm(0.999)^2*
                                                                                                                                   (av-1)))/8)/(1/qnorm(0.999)^2+1/t[1,2])/t[1,2])*100)
    t[1,7]=av*100

    for(j in 2:100) {

        t[j,1]=t[j-1]+1
        t[j,2]=max(round((actmaxd/t[j-1,2])^(1/(101-t[j,1]))*t[j-1,2]),t[j-1,2]+1)


        t[j,3]= max(0,((av*(t[j,2]/qnorm(0.975)^2+1)-sqrt((-8*av*(t[j,2]/qnorm(0.975)^2+1))^2-64*(1/qnorm(0.975)^2+1/t[j,2])*av*(t[j,2]*(av*(t[j,2]/qnorm(0.975)^2+2)-1)+qnorm(0.975)^2*
                                                                                                                                     (av-1)))/8)/(1/qnorm(0.975)^2+1/t[j,2])/t[j,2])*100)

        t[j,4]= min(100,((av*(t[j,2]/qnorm(0.975)^2+1)+sqrt((-8*av*(t[j,2]/qnorm(0.975)^2+1))^2-64*(1/qnorm(0.975)^2+1/t[j,2])*av*(t[j,2]*(av*(t[j,2]/qnorm(0.975)^2+2)-1)+qnorm(0.975)^2*
                                                                                                                                       (av-1)))/8)/(1/qnorm(0.975)^2+1/t[j,2])/t[j,2])*100)
        t[j,5]= max(0,((av*(t[j,2]/qnorm(0.999)^2+1)-sqrt((-8*av*(t[j,2]/qnorm(0.999)^2+1))^2-64*(1/qnorm(0.999)^2+1/t[j,2])*av*(t[j,2]*(av*(t[j,2]/qnorm(0.999)^2+2)-1)+qnorm(0.999)^2*
                                                                                                                                     (av-1)))/8)/(1/qnorm(0.999)^2+1/t[j,2])/t[j,2])*100)

        t[j,6]= min(100,((av*(t[j,2]/qnorm(0.999)^2+1)+sqrt((-8*av*(t[j,2]/qnorm(0.999)^2+1))^2-64*(1/qnorm(0.999)^2+1/t[j,2])*av*(t[j,2]*(av*(t[j,2]/qnorm(0.999)^2+2)-1)+qnorm(0.999)^2*
                                                                                                                                       (av-1)))/8)/(1/qnorm(0.999)^2+1/t[j,2])/t[j,2])*100)
        t[j,7]= av*100
    }
    t<-as.data.frame(t)


    # return(t)

    #t
}





#########
#
# test - delete this

dt<-read.csv("FunnelPlotTest.csv")

testnew<-phe_funnelcls_proportion(dt,x=numerator, n=denominator)

###########try again

##################################################################################
###################################################################################
#Second funtion - to append the level of significance

# create phe_proportion function using Wilson's method
phe_prop_funnel_sig <- function(data, x, n, multiplier=100) {


    # check required arguments present
    if (missing(data)|missing(x)|missing(n)) {
        stop("function phe_proportion requires at least 3 arguments: data, x, n")
    }

    # apply quotes
    x <- enquo(x)
    n <- enquo(n)

    # validate arguments
    if (any(pull(data, !!x) < 0, na.rm=TRUE)) {
        stop("numerators must be greater than or equal to zero")
    } else if (any(pull(data, !!n) <= 0, na.rm=TRUE)) {
        stop("denominators must be greater than zero")
    } else if (any(pull(data, !!x) > pull(data, !!n), na.rm=TRUE)) {
        stop("numerators must be less than or equal to denominator for a proportion statistic")
    }


    # if data is grouped then summarise
    if(!is.null(groups(data))) {
        data <- data %>%
            summarise(!!quo_name(x) := sum(!!x),
                      !!quo_name(n) := sum(!!n))
   }

    # calculate proportion and CIs
    library(dplyr)
    # initialise the variables
    # create summary variables with dplyr
    tot_x<-dt%>%
        summarise(sum(!!x))
    tot_n<-dt%>%
        summarise(sum(!!n))

    tot_av<-tot_x/tot_n*multiplier
    # write the function
    dt_sig<-dt%>%
        mutate(
            significance = case_when(
                (!!x)/(!!n)*multiplier<max(0,((tot_av/multiplier*(!!n/qnorm(0.999)^2+1)-sqrt((-8*tot_av/multiplier*(!!n/qnorm(0.999)^2+1))^2-
                                                                                       64*(1/qnorm(0.999)^2 +1/!!n)*tot_av/multiplier*(!!n*(tot_av/multiplier*(!!n/qnorm(0.999)^2+2)-1)+qnorm(0.999)^2*(tot_av/multiplier-1)))/
                                           8)/(1/qnorm(0.999)^2 +1/!!n)/!!n)*multiplier) ~ "Low (0.001)",

                !!x/!!n*multiplier<max(0,((tot_av/multiplier*(!!n/qnorm(0.975)^2+1)-sqrt((-8*tot_av/multiplier*(!!n/qnorm(0.975)^2+1))^2-
                                                                                       64*(1/qnorm(0.975)^2 +1/!!n)*tot_av/multiplier*(!!n*(tot_av/multiplier*(!!n/qnorm(0.975)^2+2)-1)+qnorm(0.975)^2*(tot_av/multiplier-1)))/
                                           8)/(1/qnorm(0.975)^2 +1/!!n)/!!n)*multiplier) ~ "Low (0.025)",

               !!x/!!n*multiplier>min(multiplier,((tot_av/multiplier*(!!n/qnorm(0.999)^2+1)+sqrt((-8*tot_av/multiplier*(!!n/qnorm(0.999)^2+1))^2-
                                                                                                64*(1/qnorm(0.999)^2 +1/!!n)*tot_av/multiplier*(!!n*(tot_av/multiplier*(!!n/qnorm(0.999)^2+2)-1)+qnorm(0.999)^2*(tot_av/multiplier-1)))/
                                                    8)/(1/qnorm(0.999)^2 +1/!!n)/!!n)*multiplier) ~ "High (0.001)",

                !!x/!!n*multiplier>min(multiplier,((tot_av/multiplier*(!!n/qnorm(0.975)^2+1)+sqrt((-8*tot_av/multiplier*(!!n/qnorm(0.975)^2+1))^2-
                                                                                                64*(1/qnorm(0.975)^2 +1/!!n)*tot_av/multiplier*(!!n*(tot_av/multiplier*(!!n/qnorm(0.975)^2+2)-1)+qnorm(0.975)^2*(tot_av/multiplier-1)))/
                                                    8)/(1/qnorm(0.975)^2 +1/!!n)/!!n)*multiplier) ~  "High (0.025)",


              TRUE ~ ""

           )
        )


}

check<-phe_prop_funnel_sig(dt,numerator,denominator,100)
