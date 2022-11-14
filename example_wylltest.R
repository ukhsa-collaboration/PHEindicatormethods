
#example of using the PYLL function to calculate WYLL  due to alcohol



library("DataLakeR")
library("dplyr")
library("PHEindicatormethods")
library("tidyverse")

years <- 2020

esp64<-c(1000,4000,5500,5500,5500,6000,6000,6500,7000,7000,7000,7000,6500,6000)
esp<-read.csv("esp13.csv") %>%
  mutate(ESP =as.numeric(ESP))

# Get all working years lost====

mort_all  <- get_mortality(Variables = c("xYear"),
                           BaseGeography = "LSOAPC",
                           RollingYears = FALSE,
                           CompleteRows = TRUE,
                           Sex = "Persons",
                           AgeBand = "SingleYear",
                           AreaTypes = c("CTRY09CD"),
                           RunQueryOnLake = TRUE,
                           filters = mortalityfilters(MinAge = 0,
                                                      MaxAge = 64,
                                                      Year = years,
                                                      CustomFilter = NULL ))



mort_all <- mort_all$data

#get deaths by 5 year age bands (di)-------------------------
mort_df_5y <-mort_all %>%
  mutate(xYear = as.numeric(xYear),
         Age_group = ifelse(Age<90,floor(Age / 5) * 5,90),
         Age_group = ifelse(Age %in% c(1,2,3,4),1,Age_group))

di2 <- mort_df_5y %>%
  group_by(AreaName,AreaCode,AreaType,xYear,Sex,Age) %>%
  mutate(YL=case_when (
    Age >= 0 & Age < 16 ~ 49,
    Age >= 16 & Age < 65 ~ 65 - Age),
    wyll = Deaths * YL) %>%
  group_by(AreaName,AreaCode,AreaType,xYear,Sex,Age_group)%>%
  summarise(Deaths=sum(Deaths),
            wyll=sum(wyll))

# Get population data (ni)----------------------------------------------------------

mid_year_pops <- get_populations(
  Variables = c("Period"),
  RunQueryOnLake = TRUE,
  YearGroupLength = NULL,
  RollingYears = FALSE,
  AgeBand = "SingleYear",
  AreaTypes = c("CTRY09CD"),
  CompleteRows = TRUE,
  FactorsAsStrings = FALSE,
  filters = Popfilters(
    MinAge = 0,
    MaxAge = 64,
    Period = years,
    SexDesc ="Persons"
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


wyll<-di2 %>%
  left_join(mype_df,
            by =c("AreaName", "AreaCode", "AreaType","Sex", "Age_group", "xYear"))%>%
  group_by(AreaName,AreaCode,AreaType,Sex,xYear) %>%
  left_join(esp, by =("Age_group")) %>%
  phe_wyll(Deaths,Population, wyll,esp64)
