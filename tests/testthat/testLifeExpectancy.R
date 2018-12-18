context("test_phe_life_expectancy")

# dps to test to
n <- 4

df1 <- data.frame(
                  startage = c(0L, 1L, 5L, 10L, 15L, 20L, 25L, 30L, 35L, 40L, 45L, 50L, 55L,
                               60L, 65L, 70L, 75L, 80L, 85L, 90L),
                  pops = c(7060L, 35059L, 46974L, 48489L, 43219L, 38561L, 46009L, 57208L,
                           61435L, 55601L, 50209L, 56416L, 46411L, 39820L, 37978L,
                           37039L, 33288L, 23306L, 11936L, 11936L),
                  deaths = c(17L, 9L, 4L, 8L, 20L, 15L, 24L, 33L, 50L, 71L, 100L, 163L,
                             263L, 304L, 536L, 872L, 1390L, 1605L, 1936L, 1937L))

df2 <- data.frame(startage = c(0L, 1L, 5L, 10L, 15L, 20L, 25L, 30L, 35L, 40L, 45L, 50L, 55L,
                               60L, 70L, 75L, 80L, 85L, 90L, 65L),
                  pops = c(7060L, 35059L, 46974L, 48489L, 43219L, 38561L, 46009L, 57208L,
                           61435L, 55601L, 50209L, 56416L, 46411L, 39820L, 37039L,
                           33288L, 23306L, 11936L, 11936L, 37978L),
                  deaths = c(17L, 9L, 4L, 8L, 20L, 15L, 24L, 33L, 50L, 71L, 100L, 163L,
                             263L, 304L, 872L, 1390L, 1605L, 1936L, 1937L, 536L))

df3 <- data.frame(startage = c("0", "1-4", "5-9", "10 – 14", "15 – 19", "20 – 24", "25 – 29",
                               "30 – 34", "35 – 39", "40 – 44", "45 – 49", "50 – 54",
                               "55 – 59", "60 – 64", "65 – 69", "75 – 79", "80 – 84",
                               "85 – 89", "90 +", "70 – 74"),
                  pops = c(7060L, 35059L, 46974L, 48489L, 43219L, 38561L, 46009L, 57208L,
                           61435L, 55601L, 50209L, 56416L, 46411L, 39820L, 37978L,
                           33288L, 23306L, 11936L, 11936L, 37039L),
                  deaths = c(17L, 9L, 4L, 8L, 20L, 15L, 24L, 33L, 50L, 71L, 100L, 163L,
                             263L, 304L, 536L, 1390L, 1605L, 1936L, 1937L, 872L))

answers1 <- round(data.frame(value = c(80.16960813, 79.36245674, 75.44193645, 70.47299936,
                                       65.52909542, 60.67510088, 55.78835987,
                                       50.92752727, 46.06740425, 41.24505636, 36.49322717,
                                       31.83343842, 27.26027992, 22.9719144, 18.76856852, 14.95858336,
                                       11.51684445, 8.618849241, 6.163942934, 6.16210635),
                             lowercl = c(79.88336642, 79.09060194, 75.17480763, 70.20749699,
                                         65.26623504, 60.41953895, 55.53881063,
                                         50.68361679, 45.82750376, 41.00928565, 36.2631218, 31.61090142,
                                         27.04436788, 22.7676441, 18.5769833, 14.78144225,
                                         11.35045933, 8.45441757, 5.984345957, 5.910935446),
                             uppercl = c(80.45584984, 79.63431154, 75.70906527, 70.73850173,
                                         65.79195581, 60.93066281, 56.03790911,
                                         51.17143776, 46.30730473, 41.48082707, 36.72333255,
                                         32.05597542, 27.47619196, 23.1761847, 18.96015373, 15.13572446,
                                         11.68322956, 8.783280913, 6.343539911,
                                         6.413277255)),
                  n)

test1 <- phe_life_expectancy(df1, deaths, pops, startage) %>%
  select(value:uppercl)
test2 <- phe_life_expectancy(df2, deaths, pops, startage) %>%
  select(value:uppercl)
test3 <- df1 %>%
  mutate(area = "test") %>%
  group_by(area) %>%
  phe_life_expectancy(deaths, pops, startage) %>%
  ungroup() %>%
  select(value:uppercl) %>%
  data.frame()
test4 <- phe_life_expectancy(df3, deaths, pops, startage,
                             age_contents = c("0", "1-4", "5-9",
                                              "10 – 14", "15 – 19",
                                              "20 – 24", "25 – 29",
                                              "30 – 34", "35 – 39",
                                              "40 – 44", "45 – 49",
                                              "50 – 54", "55 – 59",
                                              "60 – 64", "65 – 69",
                                              "70 – 74", "75 – 79",
                                              "80 – 84", "85 – 89",
                                              "90 +")) %>%
  select(value:uppercl)
test5 <- phe_life_expectancy(df1, deaths, pops, startage, le_age = 5) %>%
  select(value:uppercl)
rownames(test5) <- 3
#test calculations
test_that("LE and CIs calculate correctly",{
  expect_equal(round(test1, n), round(answers1, n),
               info = "test default")
  expect_equal(round(test2, n), round(answers1, n),
               info = "incorrect ageband order")
  expect_equal(round(test3, n), round(answers1, n),
               info = "single area grouping")
  expect_equal(round(test4, n), round(answers1, n),
               info = "custom age bands in wrong order")
  expect_equal(round(test5, n), round(answers1[3, ], n),
               info = "return single age band") ###CANT GET THIS WORKING


})

# test error handling

test_that("LE - errors are generated when invalid arguments are used",{
  expect_error(phe_life_expectancy(df3, deaths, pops, startage),
               "the contents in the startage field do not match the contents of the age_contents vector")
  expect_error(phe_life_expectancy(df3, deaths, pops, startage,
                                   age_contents = c("0", "1-4", "5-9",
                                                    "20 – 24", "25 – 29",
                                                    "10 – 14", "15 – 19",
                                                    "30 – 34", "35 – 39",
                                                    "40 – 44", "45 – 49",
                                                    "50 – 54", "55 – 59",
                                                    "60 – 64", "65 – 69",
                                                    "70 – 74", "75 – 79",
                                                    "80 – 84", "85 – 89",
                                                    "90 +")),
               "age_contents doesn't appear to be in ascending order; the following age bands appear out of position: 20 – 24, 25 – 29, 10 – 14, 15 – 19")




###test filter
df <- data.frame(
  startage = c(0L, 1L, 5L, 10L, 15L, 20L, 25L, 30L, 35L, 40L, 45L, 50L, 55L,
               60L, 65L, 70L, 75L, 80L, 85L, 90L),
  pops = c(7060L, 35059L, 46974L, 48489L, 43219L, 38561L, 46009L, 57208L,
           61435L, 55601L, 50209L, 56416L, 46411L, 39820L, 37978L,
           37039L, 33288L, 23306L, 11936L, 11936L),
  deaths = c(17L, 9L, 4L, 8L, 20L, 15L, 24L, 33L, 50L, 71L, 100L, 163L,
             263L, 304L, 536L, 872L, 1390L, 1605L, 1936L, 1937L)
)

#debug(phe_life_expectancy)
test <- phe_life_expectancy(df1, deaths, pops, startage, le_age = 5)
test <- phe_life_expectancy(df, deaths, pops, startage, le_age = c(5, 25), type = "full")

#should give warning for incorrect le_age
test <- phe_life_expectancy(df, deaths, pops, startage, le_age = 4)

df <- data.frame(
  age = c(0L, 1L, 5L, 10L, 15L, 20L, 25L, 30L, 35L, 40L, 45L, 50L, 55L,
          60L, 65L, 70L, 75L, 80L, 85L, 90L),
  pop = c(7060L, 35059L, 46974L, 48489L, 43219L, 38561L, 46009L, 57208L,
          61435L, 55601L, 50209L, 56416L, 46411L, 39820L, 37978L,
          37039L, 33288L, 23306L, 11936L, 11936L),
  deaths = c(17L, 9L, 4L, -5L, 20L, 15L, 24L, 33L, 50L, 71L, 100L, 163L,
             263L, 304L, 536L, 872L, 1390L, 1605L, 1936L, 1937L)
)

#should give warning for negative deaths
test <- phe_life_expectancy(df, deaths, pop, age)

df <- data.frame(
  age = c(0L, 1L, 5L, 10L, 15L, 20L, 25L, 30L, 35L, 40L, 45L, 50L, 55L,
          60L, 65L, 70L, 75L, 80L, 85L, 90L),
  pop = c(7060L, 35059L, 46974L, 48489L, 43219L, 0L, 46009L, 57208L,
          61435L, 55601L, 50209L, 56416L, 46411L, 39820L, 37978L,
          37039L, 33288L, 23306L, 11936L, 11936L),
  deaths = c(17L, 9L, 4L, 8L, 20L, 15L, 24L, 33L, 50L, 71L, 100L, 163L,
             263L, 304L, 536L, 872L, 1390L, 1605L, 1936L, 1937L)
)
#should give warning for zero pops
test <- phe_life_expectancy(df, deaths, pop, age)


### Test with good data, negative deaths, negative pops, missing age band, low pops and pops less than deaths
df <- data.frame(stringsAsFactors=FALSE,
                 area = c(rep("Good data", 20),
                          rep("Negative deaths", 20),
                          rep("Negative pops", 20),
                          rep("Deaths more than pops", 20),
                          rep("Low pops", 20),
                          rep("Missing age band", 19)),
                 age = c(0L, 1L, 5L, 10L, 15L, 20L, 25L, 30L, 35L, 40L, 45L, 50L, 55L,
                         60L, 65L, 70L, 75L, 80L, 85L, 90L, 0L, 1L, 5L, 10L, 15L, 20L,
                         25L, 30L, 35L, 40L, 45L, 50L, 55L, 60L, 65L, 70L, 75L, 80L, 85L,
                         90L, 0L, 1L, 5L, 10L, 15L, 20L, 25L, 30L, 35L, 40L, 45L, 50L,
                         55L, 60L, 65L, 70L, 75L, 80L, 85L, 90L, 0L, 1L, 5L, 10L, 15L,
                         20L, 25L, 30L, 35L, 40L, 45L, 50L, 55L, 60L, 65L, 70L, 75L, 80L,
                         85L, 90L, 0L, 1L, 5L, 10L, 15L, 20L, 25L, 30L, 35L, 40L, 45L, 50L,
                         55L, 60L, 65L, 70L, 75L, 80L, 85L, 90L, 0L, 1L, 5L, 10L, 15L,
                         20L, 25L, 30L, 35L, 40L, 45L, 50L, 55L, 60L, 65L, 70L, 75L, 80L,
                         85L),
                 pop = c(7060L, 35059L, 46974L, 48489L, 43219L, 43219L, 46009L, 57208L,
                         61435L, 55601L, 50209L, 56416L, 46411L, 39820L, 37978L,
                         37039L, 33288L, 23306L, 11936L, 11936L, 7060L, 35059L, 46974L, 48489L,
                         43219L, 43219L, 46009L, 57208L, 61435L, 55601L, 50209L, 56416L,
                         46411L, 39820L, 37978L, 37039L, 33288L, 23306L, 11936L, 11936L,
                         7060L, 35059L, 46974L, -10L, 43219L, 43219L, 46009L, 57208L,
                         61435L, 55601L, 50209L, 56416L, 46411L, 39820L, 37978L, 37039L,
                         33288L, 23306L, 11936L, 11936L, 7060L, 35059L, 46974L, 48489L,
                         43219L, 43219L, 46009L, 57208L, 61435L, 55601L, 50209L, 56416L,
                         46411L, 39820L, 37978L, 37039L, 33288L, 23306L, 11936L, 11936L,
                         100L, 100L, 100L, 100L, 100L, 100L, 100L, 100L, 100L, 100L, 100L,
                         100L, 100L, 100L, 100L, 100L, 100L, 100L, 100L, 100L, 7060L,
                         35059L, 46974L, 48489L, 43219L, 43219L, 46009L, 57208L, 61435L,
                         55601L, 50209L, 56416L, 46411L, 39820L, 37978L, 37039L, 33288L,
                         23306L, 11936L),
                 deaths = c(17L, 9L, 4L, 8L, 20L, 15L, 24L, 33L, 50L, 71L, 100L, 163L,
                            263L, 304L, 536L, 872L, 1390L, 1605L, 1936L, 1937L, 17L, 9L,
                            -2L, 8L, 20L, 15L, 24L, 33L, 50L, 71L, 100L, 163L, 263L, 304L,
                            536L, 872L, 1390L, 1605L, 1936L, 1937L, 17L, 9L, 4L, 8L, 20L, 15L,
                            24L, 33L, 50L, 71L, 100L, 163L, 263L, 304L, 536L, 872L, 1390L,
                            1605L, 1936L, 1937L, 17L, 9L, 4L, 8L, 20L, 50000L, 24L, 33L,
                            50L, 71L, 100L, 163L, 263L, 304L, 536L, 872L, 1390L, 1605L, 1936L,
                            1937L, 17L, 9L, 4L, 8L, 20L, 15L, 24L, 33L, 50L, 71L, 80L, 80L,
                            80L, 80L, 80L, 80L, 80L, 80L, 80L, 80L, 17L, 9L, 4L, 8L, 20L,
                            15L, 24L, 33L, 50L, 71L, 100L, 163L, 263L, 304L, 536L, 872L,
                            1390L, 1605L, 1936L)
)



###test all warnings

test2 <- df %>%
  group_by(area) %>%
  phe_life_expectancy(deaths, pop, age)

###test missing age band
df <- data.frame(stringsAsFactors=FALSE,
                 area = rep("Missing age band", 19),
                 age = c(0L, 1L, 5L, 15L, 20L, 25L, 30L, 35L, 40L, 45L, 50L, 55L, 60L,
                         65L, 70L, 75L, 80L, 85L, 90L),
                 pops = c(7060L, 35059L, 46974L, 43219L, 38561L, 46009L, 57208L, 61435L,
                          55601L, 50209L, 56416L, 46411L, 39820L, 37978L, 37039L,
                          33288L, 23306L, 11936L, 11936L),
                 deaths = c(17L, 9L, 4L, 20L, 15L, 24L, 33L, 50L, 71L, 100L, 163L, 263L,
                            304L, 536L, 872L, 1390L, 1605L, 1936L, 1937L)
)
test3 <- df %>%
  phe_life_expectancy(deaths, pops, age)

### test too many deaths
df <- data.frame(stringsAsFactors=FALSE,
                 area = c("Pops less than deaths", "Pops less than deaths",
                          "Pops less than deaths", "Pops less than deaths",
                          "Pops less than deaths", "Pops less than deaths",
                          "Pops less than deaths", "Pops less than deaths",
                          "Pops less than deaths", "Pops less than deaths", "Pops less than deaths",
                          "Pops less than deaths", "Pops less than deaths",
                          "Pops less than deaths", "Pops less than deaths",
                          "Pops less than deaths", "Pops less than deaths", "Pops less than deaths",
                          "Pops less than deaths", "Pops less than deaths"),
                 age = c(0L, 1L, 5L, 10L, 15L, 20L, 25L, 30L, 35L, 40L, 45L, 50L, 55L,
                         60L, 65L, 70L, 75L, 80L, 85L, 90L),
                 pops = c(7060L, 35059L, 46974L, 48489L, 43219L, 38561L, 46009L, 57208L,
                          61435L, 55601L, 50209L, 56416L, 46411L, 39820L, 37978L,
                          37039L, 33288L, 23306L, 11936L, 11936L),
                 deaths = c(17L, 9L, 4L, 8L, 20L, 15L, 24L, 33L, 50L, 71L, 100L, 60000L,
                            263L, 304L, 536L, 872L, 1390L, 1605L, 1936L, 1937L)
)
test4 <- df %>%
  phe_life_expectancy(deaths, pops, age)

## test low pops
df <- data.frame(stringsAsFactors=FALSE,
                 area = c("Low pops", "Low pops", "Low pops", "Low pops", "Low pops",
                          "Low pops", "Low pops", "Low pops", "Low pops",
                          "Low pops", "Low pops", "Low pops", "Low pops", "Low pops",
                          "Low pops", "Low pops", "Low pops", "Low pops", "Low pops",
                          "Low pops"),
                 age = c(0L, 1L, 5L, 10L, 15L, 20L, 25L, 30L, 35L, 40L, 45L, 50L, 55L,
                         60L, 65L, 70L, 75L, 80L, 85L, 90L),
                 pops = c(128L, 152L, 120L, 176L, 194L, 180L, 145L, 149L, 107L, 185L,
                          165L, 109L, 100L, 122L, 133L, 189L, 123L, 121L, 147L,
                          138L),
                 deaths = c(58L, 93L, 78L, 94L, 59L, 71L, 80L, 73L, 69L, 72L, 91L, 69L,
                            78L, 71L, 54L, 91L, 82L, 53L, 50L, 84L)
)
test5 <- df %>%
  phe_life_expectancy(deaths, pops, age)


