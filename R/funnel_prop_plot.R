# -------------------------------------------------------------------------------------------------
#' Calculate confidence intervals/control limits and levels of significance
#'
#' Calculates control limits adopting consistent method as per the PHE Fingertips Technical Guidance : https://fingertips.phe.org.uk/profile/guidance
#'
#' @param data a data.frame containing the data to calculate control limits for;
#'            unquoted string; no default
#'
#' @param x field name from data containing the observed numbers of cases in the sample meeting the
#'          required condition (the numerator for the CLs); unquoted string; no default
#' @param denom field name from data containing the population(s) in the sample
#'               (the denominator for the CLs); unquoted string; no default
#'
#' @return returns the original data.frame with the following appended:
#'         lower 0.025 limit, upper 0.025 limite, lower 0.001 limit, upper 0.001 limit and
#'         baseline average
#'
#' @import dplyr
#'
#' @importFrom rlang sym quo_name :=
#'
#' @examples
#'

#' @export
#'
#'
#' @family PHEindicatormethods package functions
# -------------------------------------------------------------------------------------------------

# Generate a dataframe of the confidence limits for plotting
# NB -this does not alter the original dataset but generates a dataframe of 100 records for
# plotting the control limits

phe_fun_prop_plot<-function(data,x,denom){

  # check required arguments present
  if (missing(data)|missing(x)|missing(denom)) {
    stop("function phe_proportion requires at least 3 arguments: data, x, n")
  }

  # declare a rounding function

    #calculate the initialisation variables - record level data

  prop<-data%>%
    mutate(prop = {{x}}/{{denom}})

  prop<-prop$prop
   prop_calc<-prop*100
    #aggregated data
     summaries<-data%>%
       summarise(av=sum({{x}})/sum({{denom}}), mind=min({{denom}}), maxd=max({{denom}}))
     av<-summaries$av
    mind<-summaries$mind
    maxd<-summaries$maxd
    actmind<- if(maxd>2*mind){0
    } else {floor(mind*0.95)}
    actmaxd<- ceiling((maxd*1.05)/10^(nchar(floor(maxd*1.05/10)*10)-2))*10^(nchar(floor(maxd*1.05/10)*10)-2)
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
    t[1,3]= max(0,((av*(t[1,2]/qnorm(0.975)^2+1)-sqrt((-8*av*(t[1,2]/qnorm(0.975)^2+1))^2-64*(1/qnorm(0.975)^2+1/t[1,2])*av*(t[1,2]*(av*(t[1,2]/qnorm(0.975)^2+2)-1)+qnorm(0.975)^2*                                                                                                                             (av-1)))/8)/(1/qnorm(0.975)^2+1/t[1,2])/t[1,2])*100)
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
     t[j,6]= min(100,((av*(t[j,2]/qnorm(0.999)^2+1)+sqrt((-8*av*(t[j,2]/qnorm(0.999)^2+1))^2-64*(1/qnorm(0.999)^2+1/t[j,2])*av*(t[j,2]*(av*(t[j,2]/qnorm(0.999)^2+2)-1)+qnorm(0.999)^2*                                                                                                                                 (av-1)))/8)/(1/qnorm(0.999)^2+1/t[j,2])/t[j,2])*100)
   t[j,7]= av*100
  }
t<-as.data.frame(t)

}



