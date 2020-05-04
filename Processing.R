# Download and pre-process data for the analysis.

library(tidyverse)
library(lubridate)
library(dplyr)
library(ggpubr)
library(readxl)
library(stringi)
library(tidycensus)
library(readr)
library(tidyr)
library(tigris)

# Import COVID-19 death data.
# Source: https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv
covid_deaths <- read_csv("data/covid_deaths_4_23_20.csv")

# Import COVID-19 case data.
# Source: https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv
covid_cases  <- read_csv("data/covid_cases_4_23_20.csv")

# Join data.
covid <- left_join(covid_deaths, covid_cases)
covid <- covid %>% filter(!is.na(GEOID))
covid$GEOID <- as.character(covid$GEOID)
covid$GEOID <- ifelse(nchar(covid$GEOID) == 4, paste0("0", covid$GEOID), covid$GEOID)

View(covid)

# Function to grab and pre-process census data for given year.
census_api_key("3b7f443116b03bdd7ce2f1ff3f2b117cfff19e69") 
censusGrab <- function(Year) {
  
  acsYear <- get_acs(geography = "county", 
                     variables = c(medincome = "B19013_001",
                                   population = "B01001_001",
                                   black_pop = "B01001B_001",
                                   hispanic_pop = "B01001I_001",
                                   poverty_status_total = "B17001_001",
                                   poverty_below_total = "B17001_002",
                                   median_house_value = "B25077_001",
                                   Total_Education = "B15003_001",
                                   overHS1 = "B15003_017",
                                   overHS2 = "B15003_018",
                                   overHS3 = "B15003_019",
                                   overHS4 = "B15003_020",
                                   overHS5 = "B15003_021",
                                   overHS6 = "B15003_022",
                                   overHS7 = "B15003_023",
                                   overHS8 = "B15003_024",
                                   overHS9 = "B15003_025",
                                   housing_units = "B25001_001",
                                   owner_occupied = "B25002_002",
                                   Male_age_65_66 = "B01001_020",
                                   Male_age_67_69 = "B01001_021",
                                   Male_age_70_74 = "B01001_022",
                                   Male_age_75_79 = "B01001_023",
                                   Male_age_80_84 = "B01001_024",
                                   Male_age_85_plus = "B01001_025",
                                   F_age_65_66 = "B01001_044",
                                   F_age_67_69 = "B01001_045",
                                   F_age_70_74 = "B01001_046",
                                   F_age_75_79 = "B01001_047",
                                   F_age_80_84 = "B01001_048",
                                   F_age_85_plus = "B01001_049"),
                     year = Year)
  
  acsYear <- acsYear %>% select(-moe) %>% spread(variable, estimate) %>% distinct()
  
  acsYear$year <- rep(Year, nrow(acsYear))
  
  acsYear <- acsYear %>% mutate(Black_Per = black_pop/population,
                                  Hispanic_Per = hispanic_pop/population,
                                  Poverty_Per = poverty_below_total/poverty_status_total,
                                  Under_HS_Per = 1-((overHS1 + overHS2 + overHS3 + overHS4 + overHS5 + overHS6 +
                                                       overHS7 + overHS8 +overHS9)/Total_Education),
  Owner_Occ_Per = owner_occupied/housing_units,
                                  Over64_Per = (Male_age_65_66 + Male_age_67_69 +Male_age_70_74 +
                                                  Male_age_75_79 + Male_age_80_84 + Male_age_85_plus +
                                                  F_age_65_66 + F_age_67_69 + F_age_70_74 +
                                                  F_age_75_79 + F_age_80_84 + F_age_85_plus)/population)
  
  acsYear <- acsYear %>% select(GEOID, year, median_house_value, medincome, Black_Per,
                                  Hispanic_Per, population, Poverty_Per, Under_HS_Per,
                                  Owner_Occ_Per, Over64_Per)
  
  return(acsYear)
  
}

# Pull ACS data from 2012-2018.
acslist <- lapply(2012:2018, function (x)  censusGrab(x))
acslist12_18 <- acslist[[1]]
for (i in 2:length(acslist)) acslist12_18 <- bind_rows(acslist12_18, acslist[[i]])

# Average data over years.
acsAvs <- acslist12_18 %>% group_by(GEOID) %>% 
  summarise(median_house_value_mean_12to18 = mean(median_house_value, na.rm = T),
            median_income_mean_12to18 = mean(medincome, na.rm = T),
            Black_Per_mean_12to18 = mean(Black_Per, na.rm = T),
            Hispanic_Per_mean_12to18 = mean(Hispanic_Per, na.rm =T),
            population_mean_12to18 = mean(population, na.rm =T),
            Poverty_Per_mean_12to18 = mean(Poverty_Per, na.rm = T),
            Under_HS_Per_mean_12to18 = mean(Under_HS_Per, na.rm = T),
            Owner_Occ_Per_mean_12to18 = mean(Owner_Occ_Per, na.rm = T),
            Over64_Per_mean_12to18 = mean(Over64_Per, na.rm =T))

# Pull and pre-process shapefile data.
county_shapes <- counties()
county_shapes <- as.data.frame(county_shapes)
county_shapes <- county_shapes %>% select(GEOID, ALAND, AWATER)

# Import and pre-process additional county level health data
# Source: https://www.countyhealthrankings.org/sites/default/files/media/document/Trends%20documentation%202020.pdf
county_trends <- read_csv("https://www.countyhealthrankings.org/sites/default/files/media/document/CHR_trends_csv_2020.csv")
county_trends <- county_trends %>% select(statecode, countycode, county, measurename, rawvalue)
county_trends <- county_trends %>% mutate(GEOID_state =  ifelse(nchar(statecode) == 1, paste0("0", statecode), statecode),
                                GEOID_county = ifelse(nchar(countycode) == 1, paste0("00", countycode),
                                                      ifelse(nchar(countycode) == 2, paste0("0", countycode),countycode)),
                                GEOID = paste0(GEOID_state, GEOID_county))
county_trends <- county_trends %>% filter(countycode != 0)
county_trends <- county_trends %>% group_by(GEOID, measurename) %>%
  summarise(rawvalue_mean_all_years = mean(rawvalue, na.rm = T)) 
county_trends <- county_trends %>% group_by(GEOID) %>% spread(measurename,
                                                      rawvalue_mean_all_years,
                                                      sep = "_mean_all_years_") %>% ungroup()

# Function to grab and pre-process smoking data for a given year.
# Source: https://www.countyhealthrankings.org/sites/default/files/DataDictionary_2014.pdf
smokeGrab <- function(year) {

  url <- paste0("https://www.countyhealthrankings.org/sites/default/files/analytic_data", year, ".csv")
  chr <- read_csv(url, skip=1)
  chr$GEOID <- chr$fipscode
  smoke <- chr %>% select(fipscode, county, year, v009_rawvalue)
  smoke$adultSmokeRate <- smoke$v009_rawvalue
  smoke$GEOID <- smoke$fipscode
  smoke <- smoke %>% select(GEOID, county, adultSmokeRate, year)
  return(smoke)
}

# Pull smoking data from 2010-2018.
smokelist <- lapply(2010:2018, function (x)  smokeGrab(x))
smoking_data <- smokelist[[1]]
for (i in 2:length(smokelist)) smoking_data <- bind_rows(smoking_data, smokelist[[i]])
smoking_data <- smoking_data %>% group_by(GEOID) %>% summarise( meanSmokeRate = mean(adultSmokeRate, na.rm = T) )

# Import and pre-process hospital beds data.
# Source: https://hifld-geoplatform.opendata.arcgis.com/datasets/6ac5e325468c4cb9b905f1728d6fbf0f_0
hospitals <- read_csv("https://opendata.arcgis.com/datasets/6ac5e325468c4cb9b905f1728d6fbf0f_0.csv?outSR=%7B%22latestWkid%22%3A3857%2C%22wkid%22%3A102100%7D")
hospitals$hospital_beds <- ifelse(hospitals$BEDS == -999, NA, hospitals$BEDS)
hospitals <- hospitals %>% group_by(COUNTYFIPS) %>% summarise(countyHosipitalBeds = sum(hospital_beds, na.rm = T)) %>% ungroup()
hospitals$GEOID <- hospitals$COUNTYFIPS
hospitals <- hospitals %>% select(GEOID, countyHosipitalBeds)

# Import and pre-process 2014 NATA data.
# Source: https://www.epa.gov/national-air-toxics-assessment/2014-nata-assessment-results
# Technical documentation: https://www.epa.gov/national-air-toxics-assessment/2014-nata-technical-support-document
# Variable Explinations: https://www.epa.gov/sites/production/files/2018-08/documents/explanation_of_data_elements_in_2014_nata_files.pdf
NataRespHazBySource  <- read_excel("data/nata2014v2_national_resphi_by_tract_srcgrp.xlsx")
NataRespHazByChem    <- read_excel("data/nata2014v2_national_resphi_by_tract_poll.xlsx")

nata <- NataRespHazBySource
nata <- left_join(nata, NataRespHazByChem)
nata$FIPS  = as.character(nata$FIPS)
nata$Tract = as.character(nata$Tract)
nata$FIPS_TRACT = stri_pad_right(nata$FIPS, 11, 0)
nata  <- nata [which(nata$Tract == nata$FIPS_TRACT ),]
nata$Population <- as.numeric(as.character( gsub(",", "", nata$Population) ))
nata$GEOID <- nata$FIPS

# Import and pre-process COVID-19 testing data.
# Source: https://covidtracking.com/api/v1/states/current.csv
testing <- read.csv('data/covid_testing_4_19_20.csv')
statetests <- testing %>% select(State, totaltests)

# Import and pre-process COVID-19 EJ screen data.
# Source: https://www.epa.gov/ej_screen/download-ej_screen-data
ej_screen <- read_csv("data/EJSCREEN_2019_USPR.csv")
ej_screen <- ej_screen %>% select(ACSTOTPOP,
                             ACSTOTHH,
                             MINORPOP,
                             LOWINCOME,
                             LINGISO,
                             OZONE,
                             PRE1960PCT,
                             ID) %>% mutate(GEOID = substr(ID, 1,5))
ej_screen$OZONE <- as.numeric(ej_screen$OZONE)
ej_screen_county <- ej_screen %>% group_by(GEOID) %>% 
   summarise(minority_tot_2016_county = sum(MINORPOP, na.rm =T),
             LINGISO_tot_2016_county = sum(LINGISO, na.rm =T),
             LOWINCOME_tot_2016_county = sum(LOWINCOME, na.rm =T),
             ACSTOTPOP_tot_2016_county= sum(ACSTOTPOP, na.rm = T),
             ACSTOTHH_tot_2016_county= sum(ACSTOTHH, na.rm = T),
             OZONE_mean_2016 = mean(OZONE, na.rm =T),
             PRE1960PCT_mean_13_17 = mean(PRE1960PCT, na.rm =T)) %>% ungroup()
ej_screen_county <- ej_screen_county %>% mutate(minority_per_2016_county = minority_tot_2016_county/ACSTOTPOP_tot_2016_county,
                                       LINGISO_per_2016_county = LINGISO_tot_2016_county/ACSTOTHH_tot_2016_county,
                                       LOWINCOME_per_2016_county = LOWINCOME_tot_2016_county/ACSTOTPOP_tot_2016_county)
 
ej_screen_county <- ej_screen_county %>% select(GEOID, minority_per_2016_county, LINGISO_per_2016_county, LOWINCOME_per_2016_county, OZONE_mean_2016, PRE1960PCT_mean_13_17)

# Import and pre-process temperature data.
# Source: https://wonder.cdc.gov/nasa-nldas.html
temps <- read.delim("data/North America Land Data Assimilation System (NLDAS) Daily Air Temperatures and Heat Index (1979-2011) (2).txt")
temps$GEOID <- ifelse(nchar(temps$County.Code) == 4, paste0("0", temps$County.Code), temps$County.Code)
temps <- temps %>% group_by(GEOID) %>% summarise(min_temp_11 = min(Avg.Daily.Min.Air.Temperature..C., na.rm = T),
                                                  max_temp_11 = max(Avg.Daily.Max.Air.Temperature..C., na.rm =T))
 
# Merge pre-processed data.
dfmod <- acsAvs
dfmod <- left_join(dfmod, county_trends)
dfmod <- left_join(dfmod, covid)
dfmod <- left_join(dfmod, county_shapes)
dfmod <- left_join(dfmod, smoking_data)
dfmod <- left_join(dfmod, hospitals)
dfmod <- left_join(dfmod, nata)
dfmod <- left_join(dfmod, statetests)
dfmod <- left_join(dfmod, ej_screen_county)
dfmod <- left_join(dfmod, temps)

# Additional pre-processing
# Make a population desity measure.
dfmod$ALAND <- as.numeric(dfmod$ALAND)
dfmod$ALAND <- dfmod$ALAND*.00000038610215855
dfmod$popdensity_persqmile <- dfmod$population_mean_12to18/dfmod$ALAND

# Replace NAs with zeros for covid deaths.
dfmod$cov_deaths <- ifelse(is.na(dfmod$cov_deaths), 0, dfmod$cov_deaths)

# Replace hospital bed NAs with 0.
dfmod$countyHosipitalBeds <- ifelse(is.na(dfmod$countyHosipitalBeds), 0, dfmod$countyHosipitalBeds)

# Final dataset. 
pol_county_covid <- dfmod %>% ungroup()





