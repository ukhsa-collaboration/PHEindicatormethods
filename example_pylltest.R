#example of using the PYLL function to calculate PYLL  due to alcohol

library("DataLakeR")
library("dplyr")
library("PHEindicatormethods")
library("tidyverse")

years <- 2020


# Get all potential alcohol related  years of life lost ---------------------------

mort_alc <- get_mortality(
  Variables = c("xYear"),
  RunQueryOnLake = TRUE,
  BaseGeography = "Resi",
  CountType = "Alcohol-attributable",
  YearGroupLength = NULL,
  RollingYears = FALSE,
  AgeBand = "SingleYear",
  IMD = NULL,
  Sex = "MalesFemales",
  AreaTypes = c("CTRY09CD"),
  CompleteRows = TRUE,
  FactorsAsStrings = FALSE,
  ComparabilityRatios = "automatic",
  filters = mortalityfilters(
    MinAge = 0,
    MaxAge = 150,
    Year = years,
    CustomFilter = NULL,
    ICD10UnderlyingCauseCodes = NULL,
    IncludeNeonates = TRUE,
    Alcohol = "Related",
    tables = mortality_tables()
  )
)

#SQLCodeReport(mort_alc)

mort_alc_data <- mort_alc$data

# add 5 year age bands (di)-------------------------
mort_alc_df_5y <-mort_alc_data %>%
  mutate(xYear = as.numeric(xYear),
         Age_group = ifelse(Age<90,floor(Age / 5) * 5,90),
         Age_group = ifelse(Age %in% c(1,2,3,4),1,Age_group))



di <- mort_alc_df_5y %>%
  group_by(AreaName,AreaCode,AreaType,xYear,Sex,Age_group) %>%
  summarise(
    Deaths = sum(Deaths)
  )


# Get population data (ni)----------------------------------------------------------

mid_year_pops <- get_populations(
  Variables = c("Period"),
  RunQueryOnLake = TRUE,
  YearGroupLength = NULL,
  RollingYears = FALSE,
  AgeBand = "SingleYear",
  IMD = NULL,
  AreaTypes = c("CTRY09CD"),
  CompleteRows = TRUE,
  FactorsAsStrings = FALSE,
  filters = Popfilters(
    MinAge = 0,
    MaxAge = 90,
    Period = years,
    SexDesc ="MalesFemales"
  )
)


mid_year_pops_data <- mid_year_pops$data %>%
  rename(xYear = Period) %>%
  mutate(xYear = as.numeric(xYear))

mype_df<-mid_year_pops_data %>%
  mutate(
  Age_group = ifelse(Age<90,floor(Age / 5) * 5,90),
  Age_group = ifelse(Age %in% c(1,2,3,4),1,Age_group)) %>%
  group_by(AreaName,AreaCode,AreaType, xYear,Sex, Age_group) %>%
  summarise(
    Population = sum(Population))

#add in syoa mype for very old from ONS
#https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/ageing/datasets/midyearpopulationestimatesoftheveryoldincludingcentenariansengland

df<-read.csv("englandevo2020.csv",skip = 3)

male<- df %>% slice(22:40) %>% mutate(Sex="Male")
female<- df %>% slice(42:60) %>% mutate(Sex="Female")


mype_old<-rbind(male,female) %>%
  select(2,6:22) %>%
  rename(xYear = X.1,
         X105 = X105...over) %>%
  mutate(xYear = as.numeric(xYear)) %>%
  pivot_longer(cols=X90:X105 , names_to ="Age", values_to = "Population") %>%
  mutate(Age = gsub("X","",as.character(Age)),
         Age = as.numeric(Age),
         Age = if_else(Age>=101,100,Age),
         AreaName = "England",
         AreaCode = "E92000001",
         AreaType = "CTRY09CD") %>%
  group_by(AreaName,AreaCode,AreaType, xYear,Sex,Age) %>%
  summarise(
    Population = sum(Population))

#create pop lookup to bind to ONS life tables
pop_lookup<-mid_year_pops_data %>%
  filter(AreaName=="England" & Age!=90) %>%
  rbind(mype_old)

# Add in ONS life tables LE for each age (ai) --------------------------------------

ons<-read.csv("ONS_SinlgeYr_lifeTbl_2020.csv") %>%
  mutate(xYear = as.numeric(xYear)) %>%
  filter(xYear>=2020) %>%
  left_join(pop_lookup,
            by =c("Age", "Sex", "xYear")) %>%
  mutate( LExPop =(ex * Population),
          Age_group = ifelse(Age<90,floor(Age / 5) * 5,90),
          Age_group = ifelse(Age %in% c(1,2,3,4),1,Age_group)) %>%
  group_by(AreaName,AreaCode,AreaType, xYear,Sex, Age_group) %>%
  summarise(
    Population = sum(Population),
    LExPop = sum(LExPop)) %>%
  mutate(LEadj =(LExPop / Population))


esp<-c(1000,4000,5500, 5500, 5500, 6000, 6000, 6500, 7000, 7000, 7000, 7000, 6500, 6000, 5500, 5000, 4000, 2500, 1500, 1000)


yll<-di %>%
  left_join(mype_df,
            by =c("AreaName", "AreaCode", "AreaType","Sex", "Age_group", "xYear"))%>%
  left_join(ons,
            by =c("Sex", "Age_group", "xYear")) %>%
  phe_pyll(Deaths, Population.x, LEadj, esp)

