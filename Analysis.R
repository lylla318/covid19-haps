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
                days_since_first_case,
                # date_since_social,
                POPPCT_UA,
                deathpercap,
                nataRespHaz,
                nataRespHaz_10x,
                pm25_chr,
                median_house_value_mean_12to18,
                Under_HS_Per_mean_12to18,
                Poverty_Per_mean_12to18,
                Owner_Occ_Per_mean_12to18,
                Black_Per_mean_12to18,
                Hispanic_Per_mean_12to18,
                Over64_Per_mean_12to18,
                popdensity_persqmile,
                median_income_mean_12to18,
                meanSmokeRate,
                countyHosipitalBeds,
                obesity,
                active,
                prevent_hos,
                # totaltests,
                population_mean_12to18,
                min_temp_11,
                max_temp_11,
                State) %>% distinct()

# Scale for reporting at .1 and .01 level 
pol_county_covid_mod <- pol_county_covid_mod %>%
  mutate(Allbut_top5_10x = Allbut_top5*10,
         diesel_100x = diesel*100,
         FORMALDEHYDE_10x = FORMALDEHYDE*10,
         ACETALDEHYDE_10x = ACETALDEHYDE*10,
         ACROLEIN_10x = ACROLEIN*10,
         NAPHTHALENE_100x = NAPHTHALENE*100,
         diesel_10x = diesel*10,
         NAPHTHALENE_10x = NAPHTHALENE*10)

# Eligability criteria: A county must have a covid case in order to be included in the model
removed <- pol_county_covid %>% filter(is.na(cov_cases) | cov_cases == 0)
# Note how many were revo
nrow(removed)
pol_county_covid_mod <- pol_county_covid_mod %>% filter(!is.na(cov_cases)) %>% 
  filter(cov_cases > 0)

# Remove cases with any NAs. 
pol_county_covid_mod$na_count <- apply(pol_county_covid_mod, 1, function(x) sum(is.na(x)))
pol_county_covid_mod <- pol_county_covid_mod %>% filter(na_count == 0) 

# Run model
library(glmmTMB)

#### Top5 breakout - varied 
glmmTMB.nb.t5 = glmmTMB(cov_deaths ~ Allbut_top5_10x +
                          pm25_chr + OZONE_mean_2016 +
                          FORMALDEHYDE_10x +
                          ACETALDEHYDE_10x +
                          ACROLEIN_10x +
                          NAPHTHALENE_100x +
                          diesel_100x + #pollution
                          scale(days_since_first_case) +    
                          scale(median_house_value_mean_12to18) + scale(POPPCT_UA) +
                          scale(Under_HS_Per_mean_12to18) + #social
                          scale(Poverty_Per_mean_12to18) +
                          scale(Black_Per_mean_12to18) + scale(Hispanic_Per_mean_12to18) + #demogrphic
                          scale(Over64_Per_mean_12to18) +  
                          scale(median_income_mean_12to18) +
                          scale(meanSmokeRate) +
                          scale(countyHosipitalBeds) +
                          scale(min_temp_11) + scale(max_temp_11) +
                          scale(obesity) + #health
                          scale(active) +
                          scale(prevent_hos) +
                          (1|State) +
                          offset(log(population_mean_12to18)),
                        family = nbinom2, data = pol_county_covid_mod) #political
# Create a model table with MRR and 95% confidence intervals
mysum <- summary(glmmTMB.nb.t5)
estimates <- data.frame(mysum$coefficients$cond)
estimates$var <- rownames(estimates)
estimates.t5 <- estimates %>% mutate(ci_low = Estimate - 1.96*Std..Error,
                                     ci_high = Estimate + 1.96*Std..Error, 
                                     MRR = exp(Estimate),
                                     MRR_Result = paste0(round(MRR, 2),
                                                         " (",
                                                         round(exp(ci_low),2),
                                                         " to ",
                                                         round(exp(ci_high),2),
                                                         ")"))

#### Top5 breakout - all at 10x
glmmTMB.nb.t5.10x = glmmTMB(cov_deaths ~ Allbut_top5_10x +
                              pm25_chr + OZONE_mean_2016 +
                              FORMALDEHYDE_10x +
                              ACETALDEHYDE_10x +
                              ACROLEIN_10x +
                              NAPHTHALENE_10x +
                              diesel_10x + #pollution
                              scale(days_since_first_case) +    
                              scale(median_house_value_mean_12to18) + scale(POPPCT_UA) +
                              scale(Under_HS_Per_mean_12to18) + #social
                              scale(Poverty_Per_mean_12to18) +
                              scale(Black_Per_mean_12to18) + scale(Hispanic_Per_mean_12to18) + #demogrphic
                              scale(Over64_Per_mean_12to18) +  
                              scale(median_income_mean_12to18) +
                              scale(meanSmokeRate) +
                              scale(countyHosipitalBeds) +
                              scale(min_temp_11) + scale(max_temp_11) +
                              scale(obesity) + #health
                              scale(active) +
                              scale(prevent_hos) +
                              (1|State) +
                              offset(log(population_mean_12to18)),
                            family = nbinom2, data = pol_county_covid_mod) #political
# Create a model table with MRR and 95% confidence intervals
mysum <- summary(glmmTMB.nb.t5.10x)
estimates <- data.frame(mysum$coefficients$cond)
estimates$var <- rownames(estimates)
estimates.t5.10x <- estimates %>% mutate(ci_low = Estimate - 1.96*Std..Error,
                                         ci_high = Estimate + 1.96*Std..Error, 
                                         MRR = exp(Estimate),
                                         MRR_Result = paste0(round(MRR, 2),
                                                             " (",
                                                             round(exp(ci_low),2),
                                                             " to ",
                                                             round(exp(ci_high),2),
                                                             ")"))

# Single pollutant models ########################
# Nata resp alone
glmmTMB.nb.nata = glmmTMB(cov_deaths ~ nataRespHaz_10x + 
                            #pollution
                            scale(days_since_first_case) +    
                            scale(median_house_value_mean_12to18) + scale(POPPCT_UA) +
                            scale(Under_HS_Per_mean_12to18) + #social
                            scale(Poverty_Per_mean_12to18) +
                            scale(Black_Per_mean_12to18) + scale(Hispanic_Per_mean_12to18) + #demogrphic
                            scale(Over64_Per_mean_12to18) +  
                            scale(median_income_mean_12to18) +
                            scale(meanSmokeRate) +
                            scale(countyHosipitalBeds) +
                            scale(min_temp_11) + scale(max_temp_11) +
                            scale(obesity) + #health
                            scale(active) +
                            scale(prevent_hos) +
                            (1|State) +
                            offset(log(population_mean_12to18)),
                          family = nbinom2, data = pol_county_covid_mod) #political
# Create a model table with MRR and 95% confidence intervals
mysum <- summary(glmmTMB.nb.nata)
estimates <- data.frame(mysum$coefficients$cond)
estimates$var <- rownames(estimates)
estimates.nata <- estimates %>% mutate(ci_low = Estimate - 1.96*Std..Error,
                                       ci_high = Estimate + 1.96*Std..Error, 
                                       MRR = exp(Estimate),
                                       MRR_Result = paste0(round(MRR, 2),
                                                           " (",
                                                           round(exp(ci_low),2),
                                                           " to ",
                                                           round(exp(ci_high),2),
                                                           ")"))


# Pm alone
glmmTMB.nb.pm = glmmTMB(cov_deaths ~ pm25_chr + 
                          #pollution
                          scale(days_since_first_case) +    
                          scale(median_house_value_mean_12to18) + scale(POPPCT_UA) +
                          scale(Under_HS_Per_mean_12to18) + #social
                          scale(Poverty_Per_mean_12to18) +
                          scale(Black_Per_mean_12to18) + scale(Hispanic_Per_mean_12to18) + #demogrphic
                          scale(Over64_Per_mean_12to18) +  
                          scale(median_income_mean_12to18) +
                          scale(meanSmokeRate) +
                          scale(countyHosipitalBeds) +
                          scale(min_temp_11) + scale(max_temp_11) +
                          scale(obesity) + #health
                          scale(active) +
                          scale(prevent_hos) +
                          (1|State) +
                          offset(log(population_mean_12to18)),
                        family = nbinom2, data = pol_county_covid_mod) #political
# Create a model table with MRR and 95% confidence intervals
mysum <- summary(glmmTMB.nb.pm)
estimates <- data.frame(mysum$coefficients$cond)
estimates$var <- rownames(estimates)
estimates.pm <- estimates %>% mutate(ci_low = Estimate - 1.96*Std..Error,
                                     ci_high = Estimate + 1.96*Std..Error, 
                                     MRR = exp(Estimate),
                                     MRR_Result = paste0(round(MRR, 2),
                                                         " (",
                                                         round(exp(ci_low),2),
                                                         " to ",
                                                         round(exp(ci_high),2),
                                                         ")"))

## Ozone alone
glmmTMB.nb.ozone = glmmTMB(cov_deaths ~ OZONE_mean_2016 + 
                             #pollution
                             scale(days_since_first_case) +    
                             scale(median_house_value_mean_12to18) + scale(POPPCT_UA) +
                             scale(Under_HS_Per_mean_12to18) + #social
                             scale(Poverty_Per_mean_12to18) +
                             scale(Black_Per_mean_12to18) + scale(Hispanic_Per_mean_12to18) + #demogrphic
                             scale(Over64_Per_mean_12to18) +  
                             scale(median_income_mean_12to18) +
                             scale(meanSmokeRate) +
                             scale(countyHosipitalBeds) +
                             scale(min_temp_11) + scale(max_temp_11) +
                             scale(obesity) + #health
                             scale(active) +
                             scale(prevent_hos) +
                             (1|State) +
                             offset(log(population_mean_12to18)),
                           family = nbinom2, data = pol_county_covid_mod) #political
# Create a model table with MRR and 95% confidence intervals
mysum <- summary(glmmTMB.nb.ozone)
estimates <- data.frame(mysum$coefficients$cond)
estimates$var <- rownames(estimates)
estimates.ozone <- estimates %>% mutate(ci_low = Estimate - 1.96*Std..Error,
                                        ci_high = Estimate + 1.96*Std..Error, 
                                        MRR = exp(Estimate),
                                        MRR_Result = paste0(round(MRR, 2),
                                                            " (",
                                                            round(exp(ci_low),2),
                                                            " to ",
                                                            round(exp(ci_high),2),
                                                            ")"))

#### NATA Specific Pollutants ###########
# Formaldheyde
glmmTMB.nb.Formaldheyde = glmmTMB(cov_deaths ~ FORMALDEHYDE_10x +
                                    #pollution
                                    scale(days_since_first_case) +    
                                    scale(median_house_value_mean_12to18) + scale(POPPCT_UA) +
                                    scale(Under_HS_Per_mean_12to18) + #social
                                    scale(Poverty_Per_mean_12to18) +
                                    scale(Black_Per_mean_12to18) + scale(Hispanic_Per_mean_12to18) + #demogrphic
                                    scale(Over64_Per_mean_12to18) +  
                                    scale(median_income_mean_12to18) +
                                    scale(meanSmokeRate) +
                                    scale(countyHosipitalBeds) +
                                    scale(min_temp_11) + scale(max_temp_11) +
                                    scale(obesity) + #health
                                    scale(active) +
                                    scale(prevent_hos) +
                                    (1|State) +
                                    offset(log(population_mean_12to18)),
                                  family = nbinom2, data = pol_county_covid_mod) #political
# Create a model table with MRR and 95% confidence intervals
mysum <- summary(glmmTMB.nb.Formaldheyde)
estimates <- data.frame(mysum$coefficients$cond)
estimates$var <- rownames(estimates)
estimates.Formaldheyde <- estimates %>% mutate(ci_low = Estimate - 1.96*Std..Error,
                                               ci_high = Estimate + 1.96*Std..Error, 
                                               MRR = exp(Estimate),
                                               MRR_Result = paste0(round(MRR, 2),
                                                                   " (",
                                                                   round(exp(ci_low),2),
                                                                   " to ",
                                                                   round(exp(ci_high),2),
                                                                   ")"))

# ACETALDEHYDE
glmmTMB.nb.ACETALDEHYDE = glmmTMB(cov_deaths ~ ACETALDEHYDE_10x +
                                    #pollution
                                    scale(days_since_first_case) +    
                                    scale(median_house_value_mean_12to18) + scale(POPPCT_UA) +
                                    scale(Under_HS_Per_mean_12to18) + #social
                                    scale(Poverty_Per_mean_12to18) +
                                    scale(Black_Per_mean_12to18) + scale(Hispanic_Per_mean_12to18) + #demogrphic
                                    scale(Over64_Per_mean_12to18) +  
                                    scale(median_income_mean_12to18) +
                                    scale(meanSmokeRate) +
                                    scale(countyHosipitalBeds) +
                                    scale(min_temp_11) + scale(max_temp_11) +
                                    scale(obesity) + #health
                                    scale(active) +
                                    scale(prevent_hos) +
                                    (1|State) +
                                    offset(log(population_mean_12to18)),
                                  family = nbinom2, data = pol_county_covid_mod) #political
# Create a model table with MRR and 95% confidence intervals
mysum <- summary(glmmTMB.nb.ACETALDEHYDE)
estimates <- data.frame(mysum$coefficients$cond)
estimates$var <- rownames(estimates)
estimates.ACETALDEHYDE <- estimates %>% mutate(ci_low = Estimate - 1.96*Std..Error,
                                               ci_high = Estimate + 1.96*Std..Error, 
                                               MRR = exp(Estimate),
                                               MRR_Result = paste0(round(MRR, 2),
                                                                   " (",
                                                                   round(exp(ci_low),2),
                                                                   " to ",
                                                                   round(exp(ci_high),2),
                                                                   ")"))

# ACROLEIN
glmmTMB.nb.ACROLEIN = glmmTMB(cov_deaths ~ ACROLEIN_10x +
                                #pollution
                                scale(days_since_first_case) +    
                                scale(median_house_value_mean_12to18) + scale(POPPCT_UA) +
                                scale(Under_HS_Per_mean_12to18) + #social
                                scale(Poverty_Per_mean_12to18) +
                                scale(Black_Per_mean_12to18) + scale(Hispanic_Per_mean_12to18) + #demogrphic
                                scale(Over64_Per_mean_12to18) +  
                                scale(median_income_mean_12to18) +
                                scale(meanSmokeRate) +
                                scale(countyHosipitalBeds) +
                                scale(min_temp_11) + scale(max_temp_11) +
                                scale(obesity) + #health
                                scale(active) +
                                scale(prevent_hos) +
                                (1|State) +
                                offset(log(population_mean_12to18)),
                              family = nbinom2, data = pol_county_covid_mod) #political
# Create a model table with MRR and 95% confidence intervals
mysum <- summary(glmmTMB.nb.ACROLEIN)
estimates <- data.frame(mysum$coefficients$cond)
estimates$var <- rownames(estimates)
estimates.ACROLEIN <- estimates %>% mutate(ci_low = Estimate - 1.96*Std..Error,
                                           ci_high = Estimate + 1.96*Std..Error, 
                                           MRR = exp(Estimate),
                                           MRR_Result = paste0(round(MRR, 2),
                                                               " (",
                                                               round(exp(ci_low),2),
                                                               " to ",
                                                               round(exp(ci_high),2),
                                                               ")"))



# NAPHTHALENE
glmmTMB.nb.NAPHTHALENE = glmmTMB(cov_deaths ~ NAPHTHALENE_10x +
                                   #pollution
                                   scale(days_since_first_case) +    
                                   scale(median_house_value_mean_12to18) + scale(POPPCT_UA) +
                                   scale(Under_HS_Per_mean_12to18) + #social
                                   scale(Poverty_Per_mean_12to18) +
                                   scale(Black_Per_mean_12to18) + scale(Hispanic_Per_mean_12to18) + #demogrphic
                                   scale(Over64_Per_mean_12to18) +  
                                   scale(median_income_mean_12to18) +
                                   scale(meanSmokeRate) +
                                   scale(countyHosipitalBeds) +
                                   scale(min_temp_11) + scale(max_temp_11) +
                                   scale(obesity) + #health
                                   scale(active) +
                                   scale(prevent_hos) +
                                   (1|State) +
                                   offset(log(population_mean_12to18)),
                                 family = nbinom2, data = pol_county_covid_mod) #political
# Create a model table with MRR and 95% confidence intervals
mysum <- summary(glmmTMB.nb.NAPHTHALENE)
estimates <- data.frame(mysum$coefficients$cond)
estimates$var <- rownames(estimates)
estimates.NAPHTHALENE <- estimates %>% mutate(ci_low = Estimate - 1.96*Std..Error,
                                              ci_high = Estimate + 1.96*Std..Error, 
                                              MRR = exp(Estimate),
                                              MRR_Result = paste0(round(MRR, 2),
                                                                  " (",
                                                                  round(exp(ci_low),2),
                                                                  " to ",
                                                                  round(exp(ci_high),2),
                                                                  ")"))

# NAPHTHALENE .100x
glmmTMB.nb.NAPHTHALENE.100x = glmmTMB(cov_deaths ~ NAPHTHALENE_100x +
                                        #pollution
                                        scale(days_since_first_case) +    
                                        scale(median_house_value_mean_12to18) + scale(POPPCT_UA) +
                                        scale(Under_HS_Per_mean_12to18) + #social
                                        scale(Poverty_Per_mean_12to18) +
                                        scale(Black_Per_mean_12to18) + scale(Hispanic_Per_mean_12to18) + #demogrphic
                                        scale(Over64_Per_mean_12to18) +  
                                        scale(median_income_mean_12to18) +
                                        scale(meanSmokeRate) +
                                        scale(countyHosipitalBeds) +
                                        scale(min_temp_11) + scale(max_temp_11) +
                                        scale(obesity) + #health
                                        scale(active) +
                                        scale(prevent_hos) +
                                        (1|State) +
                                        offset(log(population_mean_12to18)),
                                      family = nbinom2, data = pol_county_covid_mod) #political
# Create a model table with MRR and 95% confidence intervals
mysum <- summary(glmmTMB.nb.NAPHTHALENE.100x)
estimates <- data.frame(mysum$coefficients$cond)
estimates$var <- rownames(estimates)
estimates.NAPHTHALENE.100x <- estimates %>% mutate(ci_low = Estimate - 1.96*Std..Error,
                                                   ci_high = Estimate + 1.96*Std..Error, 
                                                   MRR = exp(Estimate),
                                                   MRR_Result = paste0(round(MRR, 2),
                                                                       " (",
                                                                       round(exp(ci_low),2),
                                                                       " to ",
                                                                       round(exp(ci_high),2),
                                                                       ")"))


# Diesel PM
glmmTMB.nb.diesel = glmmTMB(cov_deaths ~ diesel_10x +
                              #pollution
                              scale(days_since_first_case) +    
                              scale(median_house_value_mean_12to18) + scale(POPPCT_UA) +
                              scale(Under_HS_Per_mean_12to18) + #social
                              scale(Poverty_Per_mean_12to18) +
                              scale(Black_Per_mean_12to18) + scale(Hispanic_Per_mean_12to18) + #demogrphic
                              scale(Over64_Per_mean_12to18) +  
                              scale(median_income_mean_12to18) +
                              scale(meanSmokeRate) +
                              scale(countyHosipitalBeds) +
                              scale(min_temp_11) + scale(max_temp_11) +
                              scale(obesity) + #health
                              scale(active) +
                              scale(prevent_hos) +
                              (1|State) +
                              offset(log(population_mean_12to18)),
                            family = nbinom2, data = pol_county_covid_mod) #political
# Create a model table with MRR and 95% confidence intervals
mysum <- summary(glmmTMB.nb.diesel)
estimates <- data.frame(mysum$coefficients$cond)
estimates$var <- rownames(estimates)
estimates.diesel <- estimates %>% mutate(ci_low = Estimate - 1.96*Std..Error,
                                         ci_high = Estimate + 1.96*Std..Error, 
                                         MRR = exp(Estimate),
                                         MRR_Result = paste0(round(MRR, 2),
                                                             " (",
                                                             round(exp(ci_low),2),
                                                             " to ",
                                                             round(exp(ci_high),2),
                                                             ")"))
# Diesel PM 100x
glmmTMB.nb.diesel = glmmTMB(cov_deaths ~ diesel_100x +
                              #pollution
                              scale(days_since_first_case) +    
                              scale(median_house_value_mean_12to18) + scale(POPPCT_UA) +
                              scale(Under_HS_Per_mean_12to18) + #social
                              scale(Poverty_Per_mean_12to18) +
                              scale(Black_Per_mean_12to18) + scale(Hispanic_Per_mean_12to18) + #demogrphic
                              scale(Over64_Per_mean_12to18) +  
                              scale(median_income_mean_12to18) +
                              scale(meanSmokeRate) +
                              scale(countyHosipitalBeds) +
                              scale(min_temp_11) + scale(max_temp_11) +
                              scale(obesity) + #health
                              scale(active) +
                              scale(prevent_hos) +
                              (1|State) +
                              offset(log(population_mean_12to18)),
                            family = nbinom2, data = pol_county_covid_mod) #political
# Create a model table with MRR and 95% confidence intervals
mysum <- summary(glmmTMB.nb.diesel)
estimates <- data.frame(mysum$coefficients$cond)
estimates$var <- rownames(estimates)
estimates.diesel.100x <- estimates %>% mutate(ci_low = Estimate - 1.96*Std..Error,
                                              ci_high = Estimate + 1.96*Std..Error, 
                                              MRR = exp(Estimate),
                                              MRR_Result = paste0(round(MRR, 2),
                                                                  " (",
                                                                  round(exp(ci_low),2),
                                                                  " to ",
                                                                  round(exp(ci_high),2),
                                                                  ")"))