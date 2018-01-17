


test <- test_DSR_multiarea %>%
  mutate(wt_rate = test_DSR_multiarea$count * (esp2013) / test_DSR_multiarea$pop,
         sq_rate = test_DSR_multiarea$count * (esp2013/test_DSR_multiarea$pop)^2) %>%
  summarise(total_count = sum(test_DSR_multiarea$count),
            total_pop = sum(test_DSR_multiarea$pop),
            dsr = sum(wt_rate) / sum(esp2013) * 100000,
            vardsr = 1/sum(esp2013)^2 * sum(sq_rate),
            lowercl = dsr + sqrt((vardsr/sum(test_DSR_multiarea$count)))*(byars_lower(sum(test_DSR_multiarea$count),conf.level)-sum(test_DSR_multiarea$count)) * 100000,
            uppercl = dsr + sqrt((vardsr/sum(test_DSR_multiarea$count)))*(byars_upper(sum(test_DSR_multiarea$count),conf.level)-sum(test_DSR_multiarea$count)) * 100000)
