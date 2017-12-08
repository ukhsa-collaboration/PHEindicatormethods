# -------------------------------------------------------------------------------------------------

# ems package has SMR function shere can specify Byars Ci method

# -------------------------------------------------------------------------------------------------



phe_isr <- function(x,n,ref_x, ref_n, groupref = 1, conf.level = 0.95, type = "ratio", percentage = FALSE, multiplier = 1) {

# scale confidence level
  if (conf.level >= 90) {
    conf.level <- conf.level/100
  }


# calculate ISR and CIs
  if (type == "ratio") {

    # set multiplier
    if (percentage == TRUE) {
      multiplier <- 100
    }

    phe_isr <- data.frame(x,n,ref_x,ref_n,groupref) %>%
    mutate(exp_x = ref_x/ref_n * n) %>%
    group_by(groupref) %>%
    summarise(obs  = sum(x),
              exp  = sum(exp_x),
              isr  = obs / exp * multiplier) %>%
    mutate(lowercl = if_else(obs<100, qchisq((1-conf.level)/2,2*obs)/2/exp*multiplier,
                             byars_lower(obs,conf.level)/exp*multiplier),
           uppercl = if_else(obs<100, qchisq(conf.level+(1-conf.level)/2,2*obs+2)/2/exp*multiplier,
                               byars_upper(obs,conf.level)/exp*multiplier),
           method  = if_else(obs<100,"Exact","Byars"))

  names(phe_isr) <- c("group", "observed", "expected", "isr",
                      paste("lower",conf.level*100,"cl",sep=""),
                      paste("upper",conf.level*100,"cl",sep=""),"method")
  }

if (type == "rate") {

    phe_isr <- data.frame(x,n,ref_x,ref_n,groupref) %>%
      mutate(exp_x = ref_x/ref_n * n) %>%
      group_by(groupref) %>%
      summarise(obs  = sum(x),
                exp  = sum(exp_x),
                ref_rate = sum(ref_x) / sum(ref_n) * multiplier,
                isr  = obs / exp * ref_rate) %>%
      mutate(lowercl = if_else(obs<100, qchisq((1-conf.level)/2,2*obs)/2/exp * ref_rate,
                               byars_lower(obs,conf.level)/exp * ref_rate),
             uppercl = if_else(obs<100, qchisq(conf.level+(1-conf.level)/2,2*obs+2)/2/exp * ref_rate,
                               byars_upper(obs,conf.level)/exp * ref_rate),
             method  = if_else(obs<100,"Exact","Byars"))

    names(phe_isr) <- c("group", "observed", "expected", "reference rate", "isr",
                        paste("lower",conf.level*100,"cl",sep=""),
                        paste("upper",conf.level*100,"cl",sep=""),"method")

  }

  return(phe_isr)
}

