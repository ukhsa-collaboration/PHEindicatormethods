# -------------------------------------------------------------------------------------------------

# ems package has SMR function shere can specify Byars Ci method

# -------------------------------------------------------------------------------------------------

# Load test data
bigdata          <- read_excel(".\\tests\\testthat\\testdata_ISR.xlsx", sheet="testdata_big", col_names=TRUE)
smalldata        <- read_excel(".\\tests\\testthat\\testdata_ISR.xlsx", sheet="testdata_small", col_names=TRUE)
multiareadata    <- read_excel(".\\tests\\testthat\\testdata_ISR.xlsx", sheet="testdata_multiarea", col_names=TRUE)
testdata_results <- read_excel(".\\tests\\testthat\\testdata_ISR.xlsx", sheet="testresults", col_names=TRUE)
refdata          <- read_excel(".\\tests\\testthat\\testdata_ISR.xlsx", sheet="refdata", col_names=TRUE)


phe_isr <- function(x,n,ref_x, ref_n, groupref = 1, conf.level = 0.95, percentage = FALSE) {

# scale confidence level
  if (conf.level >= 90) {
    conf.level <- conf.level/100
  }

# calculate ISR and CIs
  phe_isr <- data.frame(x, n, ref_x, ref_n, groupref) %>%
    group_by(groupref) %>%
    mutate(pred_x = ref_x/ref_n * n) %>%
    summarise(total_count = sum(x),
              total_pop = sum(n),
              dsr = sum(wt_rate) / sum(stdpop) * multiplier,
              vardsr = 1/sum(stdpop)^2 * sum(sq_rate),
              lowercl = dsr + sqrt((vardsr/sum(x)))*(byars_lower(sum(x),conf.level)-sum(x)) * multiplier,
              uppercl = dsr + sqrt((vardsr/sum(x)))*(byars_upper(sum(x),conf.level)-sum(x)) * multiplier) %>%
    mutate(method = "Dobson") %>%
    select(method, groupref, total_count, total_pop, dsr, lowercl, uppercl)


  names(phe_dsr) <- c("method", "group", "total_count", "total_pop", "dsr",
                      paste("lower",conf.level*100,"cl",sep=""),
                      paste("upper",conf.level*100,"cl",sep=""))

  return(phe_dsr)


}


phe_isr <- data.frame(count = multiareadata$count,
                      pop = multiareadata$pop,
                      refcount = refdata$refcount,
                      refpop = refdata$refpop,
                      group = multiareadata$area) %>%
  mutate(pred_x = refcount/refpop * pop) %>%
  group_by(group) %>%
  summarise(obs = sum(count),
            exp = sum(pred_x),
            isr = obs / exp * 100,
            lowercl_tmp <- byars_lower(obs,0.95)/exp*100,
            uppercl_tmp <- byars_upper(obs,0.95)/exp*100)
  mutate(method = "Dobson") %>%
  select(method, groupref, total_count, total_pop, dsr, lowercl, uppercl)

}



#SMR.table(multiareadata, group.var="area", obs.var="count", pred.var="pred_var",
#          digits = 5, use.label = FALSE, var.labels = attr(data, "var.labels"),
#          ci.method = "Byar", ci.level = 0.95, reorder = c("no","SMR", "lower.Cl", "upper.Cl"),
#          decreasing = FALSE)

