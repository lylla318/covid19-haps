# Code to analyze data for our manuscript

# Remove diesel from the analysis for testing
pol_county_covid <- pol_county_covid %>% mutate(AllbutDeisel = rowSums(dplyr::select(.,79:94,96:121)))
pol_county_covid <- pol_county_covid %>% mutate(Allbut_top5andDeisel = rowSums(dplyr::select(.,79:83, 86:91, 93,94,96:99, 101:109,111:121)))
pol_county_covid <- pol_county_covid %>% mutate(Allbut_top6 = rowSums(dplyr::select(.,79:83, 86:91, 93,94,96:99, 101:109,111:121)))  

# Update variable names.
pol_county_covid$deathpercap <- pol_county_covid$cov_deaths/pol_county_covid$Population
pol_county_covid$nataRespHaz <- pol_county_covid$`Total Respiratory (hazard quotient)`
pol_county_covid$nataRespHaz_10x <- pol_county_covid$`Total Respiratory (hazard quotient)`*10
pol_county_covid$nataPT <- pol_county_covid$`PT-StationaryPoint Respiratory (hazard quotient)`  
pol_county_covid$diesel <- pol_county_covid$`DIESEL PM` 
pol_county_covid$active <-pol_county_covid$`measurename_mean_all_years_Physical inactivity`
pol_county_covid$prevent_hos <- pol_county_covid$`measurename_mean_all_years_Preventable hospital stays`
pol_county_covid$pm25_chr <- pol_county_covid$`measurename_mean_all_years_Air pollution - particulate matter`
pol_county_covid$obesity <- pol_county_covid$`measurename_mean_all_years_Adult obesity`


# Select the variables for modeling
pol_county_covid_mod <- pol_county_covid %>% 
  dplyr::select(GEOID,
         FORMALDEHYDE,
         ACETALDEHYDE,
         ACROLEIN,
         NAPHTHALENE,
         CHLORINE,
         diesel,
         Allbut_top5,
         OZONE_mean_2016,
         LINGISO_per_2016_county,
         cov_cases,
         cov_deaths,
         deathpercap,
         nataRespHaz_10x,
         pm25_chr,
         median_house_value_mean_12to18,
         Under_HS_Per_mean_12to18,
         Poverty_Per_mean_12to18,
         Owner_Occ_Per_mean_12to18,
         Black_Per_mean_12to18,
         Hispanic_Per_mean_12to18,
         Over64_Per_mean_12to18,
         POPPCT_UA,
         median_income_mean_12to18,
         meanSmokeRate,
         countyHosipitalBeds,
         obesity,
         active,
         prevent_hos,
         totaltests,
         population_mean_12to18,
         min_temp_11,
         max_temp_11,
         State) %>% distinct()

# Scale data for reporting
pol_county_covid_mod <- pol_county_covid_mod  %>% mutate(Allbut_top5_10x = Allbut_top5*10,
                                                    diesel_100x = diesel*100,
                                                    FORMALDEHYDE_10x = FORMALDEHYDE*10,
                                                    ACETALDEHYDE_10x = ACETALDEHYDE*10,
                                                    ACROLEIN_10x = ACROLEIN*10,
                                                    NAPHTHALENE_100x = NAPHTHALENE*100,
                                                    CHLORINE_10x = CHLORINE*10)

pol_county_covid_mod <- pol_county_covid_mod  %>% mutate(diesel_10x = diesel*10,
                                                         NAPHTHALENE_10x = NAPHTHALENE*10)


# Eligability criteria: A county must have a covid case in order to be included in the model
removed <- pol_county_covid %>% filter(is.na(cov_cases) | cov_cases == 0)
nrow(removed)

pol_county_covid_mod <- pol_county_covid_mod %>% filter(!is.na(cov_cases)) %>% 
  filter(cov_cases > 0)

# Remove cases with any NAs. 
pol_county_covid_mod$na_count <- apply(pol_county_covid_mod, 1, function(x) sum(is.na(x)))

pol_county_covid_mod <- pol_county_covid_mod %>% filter(na_count == 0) 

# Run model
library(NBZIMM)
library(lme4)
library(nlme)

# Main model - zero inflated negative binomial mixed model state fixed
glmm.zinb.off = glmm.zinb(cov_deaths ~ nataRespHaz_10x +
                            pm25_chr +
                            OZONE_mean_2016 + #pollution
                            scale(median_house_value_mean_12to18) +
                            scale(Under_HS_Per_mean_12to18) + #social
                            scale(Poverty_Per_mean_12to18) +
                            scale(LINGISO_per_2016_county) +
                            scale(Black_Per_mean_12to18) + scale(Hispanic_Per_mean_12to18) + #demogrphic
                            scale(Over64_Per_mean_12to18) +  
                            scale(median_income_mean_12to18) +
                            scale(POPPCT_UA) +
                            scale(meanSmokeRate) +
                            scale(countyHosipitalBeds) + scale(min_temp_11) + scale(max_temp_11) +
                            scale(obesity) + #health
                            scale(active) +
                            scale(prevent_hos) +
                            scale(totaltests) +
                            offset(log(population_mean_12to18)),
                          random = ~ 1 | State, data = (pol_county_covid_mod)) #political
summary(glmm.zinb.off)
exp(glmm.zinb.off$coefficients$fixed)


# Top 6 breakout
glmm.zinb.off.d.t5 = glmm.zinb(cov_deaths ~ Allbut_top5_10x +
                                 pm25_chr + OZONE_mean_2016 +
                                 FORMALDEHYDE_10x +
                                 ACETALDEHYDE_10x +
                                 ACROLEIN_10x +
                                 NAPHTHALENE_100x +
                                 diesel_100x + #pollution
                                 scale(median_house_value_mean_12to18) +
                                 scale(Under_HS_Per_mean_12to18) + #social
                                 scale(Poverty_Per_mean_12to18) +
                                 scale(LINGISO_per_2016_county) +
                                 scale(Black_Per_mean_12to18) + scale(Hispanic_Per_mean_12to18) + #demogrphic
                                 scale(Over64_Per_mean_12to18) +  
                                 scale(median_income_mean_12to18) +
                                 scale(POPPCT_UA) +
                                 scale(meanSmokeRate) +
                                 scale(countyHosipitalBeds) + scale(min_temp_11) + scale(max_temp_11) +
                                 scale(obesity) + #health
                                 scale(active) +
                                 scale(prevent_hos) +
                                 scale(totaltests) +
                                 offset(log(population_mean_12to18)),
                               random = ~ 1 | State, data = (pol_county_covid_mod)) #political
summary(glmm.zinb.off.d.t5)
exp(glmm.zinb.off.d.t5$coefficients$fixed)


## ISO pollutant models 

# NATA respiratory hazard alone
glmm.zinb.off.nata = glmm.zinb(cov_deaths ~ nataRespHaz_10x +
                           #pollution
                            scale(median_house_value_mean_12to18) +
                            scale(Under_HS_Per_mean_12to18) + #social
                            scale(Poverty_Per_mean_12to18) +
                            scale(LINGISO_per_2016_county) +
                            scale(Black_Per_mean_12to18) + scale(Hispanic_Per_mean_12to18) + #demogrphic
                            scale(Over64_Per_mean_12to18) +  
                            scale(median_income_mean_12to18) +
                            scale(POPPCT_UA) +
                            scale(meanSmokeRate) +
                            scale(countyHosipitalBeds) + scale(min_temp_11) + scale(max_temp_11) +
                            scale(obesity) + #health
                            scale(active) +
                            scale(prevent_hos) +
                            scale(totaltests) +
                            offset(log(population_mean_12to18)),
                          random = ~ 1 | State, data = (pol_county_covid_mod)) #political
summary(glmm.zinb.off.nata)


# PM2.5 alone
glmm.zinb.off.pm = glmm.zinb(cov_deaths ~ 
                               pm25_chr + #pollution
                               scale(median_house_value_mean_12to18) +
                               scale(Under_HS_Per_mean_12to18) + #social
                               scale(Poverty_Per_mean_12to18) +
                               scale(LINGISO_per_2016_county) +
                               scale(Black_Per_mean_12to18) + scale(Hispanic_Per_mean_12to18) + #demogrphic
                               scale(Over64_Per_mean_12to18) +  
                               scale(median_income_mean_12to18) +
                               scale(POPPCT_UA) +
                               scale(meanSmokeRate) +
                               scale(countyHosipitalBeds) + scale(min_temp_11) + scale(max_temp_11) +
                               scale(obesity) + #health
                               scale(active) +
                               scale(prevent_hos) +
                               scale(totaltests) +
                               offset(log(population_mean_12to18)),
                             random = ~ 1 | State, data = (pol_county_covid_mod)) #political
summary(glmm.zinb.off.nata)
exp(glmm.zinb.off$coefficients$fixed)

# Ozone alone
glmm.zinb.off.ozone = glmm.zinb(cov_deaths ~ 
            OZONE_mean_2016 + #pollution
            scale(median_house_value_mean_12to18) +
            scale(Under_HS_Per_mean_12to18) + #social
            scale(Poverty_Per_mean_12to18) +
            scale(LINGISO_per_2016_county) +
            scale(Black_Per_mean_12to18) + scale(Hispanic_Per_mean_12to18) + #demogrphic
            scale(Over64_Per_mean_12to18) +  
            scale(median_income_mean_12to18) +
            scale(POPPCT_UA) +
            scale(meanSmokeRate) +
            scale(countyHosipitalBeds) + scale(min_temp_11) + scale(max_temp_11) +
            scale(obesity) + #health
            scale(active) +
            scale(prevent_hos) +
            scale(totaltests) +
            offset(log(population_mean_12to18)),
          random = ~ 1 | State, data = (pol_county_covid_mod)) #political
summary(glmm.zinb.off.ozone)

## Chemicals specific to NATA
                                       
# Formaldheyde
glmm.zinb.off.FORMALDEHYDE = glmm.zinb(cov_deaths ~ 
                                 FORMALDEHYDE_10x +
                                 scale(median_house_value_mean_12to18) +
                                 scale(Under_HS_Per_mean_12to18) + #social
                                 scale(Poverty_Per_mean_12to18) +
                                 scale(LINGISO_per_2016_county) +
                                 scale(Black_Per_mean_12to18) + scale(Hispanic_Per_mean_12to18) + #demogrphic
                                 scale(Over64_Per_mean_12to18) +  
                                 scale(median_income_mean_12to18) +
                                 scale(POPPCT_UA) +
                                 scale(meanSmokeRate) +
                                 scale(countyHosipitalBeds) + scale(min_temp_11) + scale(max_temp_11) +
                                 scale(obesity) + #health
                                 scale(active) +
                                 scale(prevent_hos) +
                                 scale(totaltests) +
                                 offset(log(population_mean_12to18)),
                               random = ~ 1 | State, data = (pol_county_covid_mod)) #political
summary(glmm.zinb.off.for)

# Acetaldehyde
glmm.zinb.off.ACETALDEHYDE = glmm.zinb(cov_deaths ~ 
                                 ACETALDEHYDE_10x +
                                 scale(median_house_value_mean_12to18) +
                                 scale(Under_HS_Per_mean_12to18) + #social
                                 scale(Poverty_Per_mean_12to18) +
                                 scale(LINGISO_per_2016_county) +
                                 scale(Black_Per_mean_12to18) + scale(Hispanic_Per_mean_12to18) + #demogrphic
                                 scale(Over64_Per_mean_12to18) +  
                                 scale(median_income_mean_12to18) +
                                 scale(POPPCT_UA) +
                                 scale(meanSmokeRate) +
                                 scale(countyHosipitalBeds) + scale(min_temp_11) + scale(max_temp_11) +
                                 scale(obesity) + #health
                                 scale(active) +
                                 scale(prevent_hos) +
                                 scale(totaltests) +
                                 offset(log(population_mean_12to18)),
                               random = ~ 1 | State, data = (pol_county_covid_mod)) #political
summary(glmm.zinb.off.ACETALDEHYDE)


# Acrolein
glmm.zinb.off.ACROLEIN = glmm.zinb(cov_deaths ~ 
                                 ACROLEIN_10x + #pollution
                                 scale(median_house_value_mean_12to18) +
                                 scale(Under_HS_Per_mean_12to18) + #social
                                 scale(Poverty_Per_mean_12to18) +
                                 scale(LINGISO_per_2016_county) +
                                 scale(Black_Per_mean_12to18) + scale(Hispanic_Per_mean_12to18) + #demogrphic
                                 scale(Over64_Per_mean_12to18) +  
                                 scale(median_income_mean_12to18) +
                                 scale(POPPCT_UA) +
                                 scale(meanSmokeRate) +
                                 scale(countyHosipitalBeds) + scale(min_temp_11) + scale(max_temp_11) +
                                 scale(obesity) + #health
                                 scale(active) +
                                 scale(prevent_hos) +
                                 scale(totaltests) +
                                 offset(log(population_mean_12to18)),
                               random = ~ 1 | State, data = (pol_county_covid_mod)) #political
summary(glmm.zinb.off.ACROLEIN)


# Naphthalene
glmm.zinb.off.NAPHTHALENE = glmm.zinb(cov_deaths ~ 
                                 NAPHTHALENE_100x +
                                 scale(median_house_value_mean_12to18) +
                                 scale(Under_HS_Per_mean_12to18) + #social
                                 scale(Poverty_Per_mean_12to18) +
                                 scale(LINGISO_per_2016_county) +
                                 scale(Black_Per_mean_12to18) + scale(Hispanic_Per_mean_12to18) + #demogrphic
                                 scale(Over64_Per_mean_12to18) +  
                                 scale(median_income_mean_12to18) +
                                 scale(POPPCT_UA) +
                                 scale(meanSmokeRate) +
                                 scale(countyHosipitalBeds) + scale(min_temp_11) + scale(max_temp_11) +
                                 scale(obesity) + #health
                                 scale(active) +
                                 scale(prevent_hos) +
                                 scale(totaltests) +
                                 offset(log(population_mean_12to18)),
                               random = ~ 1 | State, data = (pol_county_covid_mod)) #political
summary(glmm.zinb.off.NAPHTHALENE)


# Diesel
glmm.zinb.off.diesel = glmm.zinb(cov_deaths ~ 
                                 diesel_100x + #pollution
                                 scale(median_house_value_mean_12to18) +
                                 scale(Under_HS_Per_mean_12to18) + #social
                                 scale(Poverty_Per_mean_12to18) +
                                 scale(LINGISO_per_2016_county) +
                                 scale(Black_Per_mean_12to18) + scale(Hispanic_Per_mean_12to18) + #demogrphic
                                 scale(Over64_Per_mean_12to18) +  
                                 scale(median_income_mean_12to18) +
                                 scale(POPPCT_UA) +
                                 scale(meanSmokeRate) +
                                 scale(countyHosipitalBeds) + scale(min_temp_11) + scale(max_temp_11) +
                                 scale(obesity) + #health
                                 scale(active) +
                                 scale(prevent_hos) +
                                 scale(totaltests) +
                                 offset(log(population_mean_12to18)),
                               random = ~ 1 | State, data = (pol_county_covid_mod)) #political
summary(glmm.zinb.off.diesel)

                                       
## ISO pollutant models at 10x

# NAPHTHALENE
glmm.zinb.off.NAPHTHALENE.b = glmm.zinb(cov_deaths ~ 
                                          NAPHTHALENE_10x +
                                          scale(median_house_value_mean_12to18) +
                                          scale(Under_HS_Per_mean_12to18) + #social
                                          scale(Poverty_Per_mean_12to18) +
                                          scale(LINGISO_per_2016_county) +
                                          scale(Black_Per_mean_12to18) + scale(Hispanic_Per_mean_12to18) + #demogrphic
                                          scale(Over64_Per_mean_12to18) +  
                                          scale(median_income_mean_12to18) +
                                          scale(POPPCT_UA) +
                                          scale(meanSmokeRate) +
                                          scale(countyHosipitalBeds) + scale(min_temp_11) + scale(max_temp_11) +
                                          scale(obesity) + #health
                                          scale(active) +
                                          scale(prevent_hos) +
                                          scale(totaltests) +
                                          offset(log(population_mean_12to18)),
                                        random = ~ 1 | State, data = (pol_county_covid_mod)) #political



# Diesel
glmm.zinb.off.diesel.b = glmm.zinb(cov_deaths ~ 
                                     diesel_10x + #pollution
                                     scale(median_house_value_mean_12to18) +
                                     scale(Under_HS_Per_mean_12to18) + #social
                                     scale(Poverty_Per_mean_12to18) +
                                     scale(LINGISO_per_2016_county) +
                                     scale(Black_Per_mean_12to18) + scale(Hispanic_Per_mean_12to18) + #demogrphic
                                     scale(Over64_Per_mean_12to18) +  
                                     scale(median_income_mean_12to18) +
                                     scale(POPPCT_UA) +
                                     scale(meanSmokeRate) +
                                     scale(countyHosipitalBeds) + scale(min_temp_11) + scale(max_temp_11) +
                                     scale(obesity) + #health
                                     scale(active) +
                                     scale(prevent_hos) +
                                     scale(totaltests) +
                                     offset(log(population_mean_12to18)),
                                   random = ~ 1 | State, data = (pol_county_covid_mod)) #political
summary(glmm.zinb.off.diesel)

# All at the 10x level 
glmm.zinb.off.d.t5.10 = glmm.zinb(cov_deaths ~ Allbut_top5_10x +
                                 pm25_chr + OZONE_mean_2016 +
                                 FORMALDEHYDE_10x +
                                 ACETALDEHYDE_10x +
                                 ACROLEIN_10x +
                                 NAPHTHALENE_10x +
                                 diesel_10x + #pollution
                                 scale(median_house_value_mean_12to18) +
                                 scale(Under_HS_Per_mean_12to18) + #social
                                 scale(Poverty_Per_mean_12to18) +
                                 scale(LINGISO_per_2016_county) +
                                 scale(Black_Per_mean_12to18) + scale(Hispanic_Per_mean_12to18) + #demogrphic
                                 scale(Over64_Per_mean_12to18) +  
                                 scale(median_income_mean_12to18) +
                                 scale(POPPCT_UA) +
                                 scale(meanSmokeRate) +
                                 scale(countyHosipitalBeds) + scale(min_temp_11) + scale(max_temp_11) +
                                 scale(obesity) + #health
                                 scale(active) +
                                 scale(prevent_hos) +
                                 scale(totaltests) +
                                 offset(log(population_mean_12to18)),
                               random = ~ 1 | State, data = (pol_county_covid_mod)) #political

# Unscaled
glmm.zinb.off.d.t5.10.us = glmm.zinb(cov_deaths ~ Allbut_top5_10x +
                                    pm25_chr + OZONE_mean_2016 +
                                    FORMALDEHYDE_10x +
                                    ACETALDEHYDE_10x +
                                    ACROLEIN_10x +
                                    NAPHTHALENE_10x +
                                    diesel_10x + #pollution
                                    log(median_house_value_mean_12to18) +
                                    Under_HS_Per_mean_12to18 + #social
                                    Poverty_Per_mean_12to18 +
                                    LINGISO_per_2016_county +
                                    Black_Per_mean_12to18 + Hispanic_Per_mean_12to18 + #demogrphic
                                    Over64_Per_mean_12to18 +  
                                    log(median_income_mean_12to18) +
                                    POPPCT_UA +
                                    meanSmokeRate +
                                    countyHosipitalBeds + min_temp_11 + max_temp_11 +
                                    obesity + #health
                                    active +
                                    prevent_hos +
                                    totaltests +
                                    offset(log(population_mean_12to18)),
                                  random = ~ 1 | State, data = (pol_county_covid_mod)) #political




