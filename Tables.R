# Generate values and tables for the manuscript.

# How many deaths?
sum(pol_county_covid$cov_deaths)
# How many counties
nrow(pol_county_covid)
# How many 0 cases do we have, 
no_covid_deaths <- pol_county_covid %>% filter(cov_deaths == 0)
nrow(no_covid_deaths)
no_covid_cases <- pol_county_covid %>% filter(cov_cases == 0)
nrow(no_covid_deaths)

#create a descriptive statistics table 

pol_county_covid_tab <- pol_county_covid_mod %>% 
  transmute("Mean of block group ozone summer seasonal avg. of daily maximum 8-hour concentration in air in parts per billion" = OZONE_mean_2016,
            #"Percent of people in a county living in linguistically isolated households 2016" = LINGISO_per_2016_county*100,
            "Covid-19 deaths up-to July 11th" = cov_deaths,
            "NATA respiratory hazard index 2014" = nataRespHaz_10x/10,
            "Formaldehyde NATA respiratory hazard quotient 2014" = FORMALDEHYDE,
            "Acetaldehyde NATA respiratory hazard quotient 2014" = 	ACETALDEHYDE,
            "Acrolein	NATA respiratory hazard quotient 2014" = ACROLEIN,
            "Naphthalene NATA respiratory hazard quotient 2014" = NAPHTHALENE,
            "Diesel PM NATA respiratory hazard quotient 2014" = diesel,
            "Average PM2.5 concentration 2010-2012" = pm25_chr,
            "Average median home value 2012-2018" = median_house_value_mean_12to18,
            "Average percentage of individuals with below high school education value 2012-2018" = Under_HS_Per_mean_12to18*100,
            "Average percentage of individuals below poverty line 2012-2018" = Poverty_Per_mean_12to18*100,
            "Average percent black 2012-2018" = Black_Per_mean_12to18*100,
            "Average percent hispanic 2012-2018" = Hispanic_Per_mean_12to18*100,
            "Average percent over 64 years old 2012-2018" = Over64_Per_mean_12to18*100,
            "Percent population living inside urban areas 2010" = POPPCT_UA,
            "Average population smoking rate 2010-2018" = meanSmokeRate*100,
            "Total county hospital beds 2019" = countyHosipitalBeds,
            "County monthly average daily maximum temperature 2011 celsius" = max_temp_11,
            "County monthly average daily minimum temperature 2011 celsius" = min_temp_11,
            "Average percentage of adults that report a BMI of 30 or more 2004-2016" = obesity*100,
            "Average percentage of adults aged 20 and over reporting no leisure-time physical activity 2004-2016" = active*100,
            "Average hospital discharge percentage for ambulatory care-sensitive conditions fee-forservice Medicare enrollees 2012-2017" = prevent_hos/1000,
            "Total days since first reported COVID-19 case" = days_since_first_case,
            "Average population 2012-2018" = population_mean_12to18)

var.names <- names(pol_county_covid_tab)
var.sd <-sapply(pol_county_covid_tab, sd, na.rm = TRUE)
var.mean <-sapply(pol_county_covid_tab, mean, na.rm = TRUE)
var.median <-sapply(pol_county_covid_tab, median, na.rm = TRUE)
var.max <-sapply(pol_county_covid_tab, max, na.rm = TRUE)
var.min <-sapply(pol_county_covid_tab, min, na.rm = TRUE)

desc_stats <- data.frame(var.names, var.mean, var.sd, var.median, var.max, var.min)

desc_stats$summary <- paste0(round(desc_stats$var.mean, 2), " (", round(desc_stats$var.sd, 2), ")")

desctable <- desc_stats %>% transmute(var.names = var.names,
                                      mean = round(var.mean, 2),
                                      sd = round(var.sd, 2),
                                      median = round(var.median, 2),
                                      range = paste0("(",round(var.min, 2),  ", ", round(var.max, 2), ")"))

write.csv(desctable, "covid_haps_descriptive_statitics.csv")

# Table 2: Summarize single and multi pollution model output.

# Multi with 10 and 100x ############################################
Pollutant <- c("All Other HAPS (.1 RQ)",
               "PM25 (1ug/m^3)",
               "Ozone (1ppb)",
               "Formaldehyde (.1 RQ)",                                          
               "Acetaldehyde (.1 RQ)",                                          
               "Acrolein (.1 RQ)",                                               
               "Naphthalene (.01 RQ)",
               "Diesel PM (.01 RQ)")

MRR <- estimates.t5$MRR[2:9]
CIlow <- exp(estimates.t5$ci_low[2:9])
CIH <- exp(estimates.t5$ci_high[2:9])
results <- estimates.t5$MRR_Result[2:9]
MRR.df.d.t5 <- data.frame(Pollutant, MRR, CIlow, CIH, results)

# Multi with only 10x #########################################
Pollutant <- c("All Other HAPS (.1 RQ)",
               "PM25 (1ug/m^3)",
               "Ozone (1ppb)",
               "Formaldehyde (.1 RQ)",                                          
               "Acetaldehyde (.1 RQ)",                                          
               "Acrolein (.1 RQ)",                                               
               "Naphthalene (.1 RQ)",
               "Diesel PM (.1 RQ)")

MRR <- estimates.t5.10x$MRR[2:9]
CIlow <- exp(estimates.t5.10x$ci_low[2:9])
CIH <- exp(estimates.t5.10x$ci_high[2:9])
results <- estimates.t5.10x$MRR_Result[2:9]
MRR.df.d.t5.10x <- data.frame(Pollutant, MRR, CIlow, CIH, results)

# single 10x ##############################################################################
Pollutant <- c("PM25 (1ug/m^3)",
               "Ozone (1ppb)",
               "All Resp. HAPS (.1 RQ)",
               "Formaldehyde (.1 RQ)",                                          
               "Acetaldehyde (.1 RQ)",                                          
               "Acrolein (.1 RQ)",                                               
               "Naphthalene (.1 RQ)",
               "Diesel PM (.1 RQ)")

MRR <- c(estimates.pm$MRR[2],
         estimates.ozone$MRR[2],
         estimates.nata$MRR[2],
         estimates.Formaldheyde$MRR[2],
         estimates.ACETALDEHYDE$MRR[2],
         estimates.ACROLEIN$MRR[2],
         estimates.NAPHTHALENE$MRR[2],
         estimates.diesel$MRR[2])

MRR_Result <- c(estimates.pm$MRR_Result[2],
                estimates.ozone$MRR_Result[2],
                estimates.nata$MRR_Result[2],
                estimates.Formaldheyde$MRR_Result[2],
                estimates.ACETALDEHYDE$MRR_Result[2],
                estimates.ACROLEIN$MRR_Result[2],
                estimates.NAPHTHALENE$MRR_Result[2],
                estimates.diesel$MRR_Result[2])
MRR.df.indv.10x <- data.frame(Pollutant, MRR, MRR_Result)

# single with 10 and 100x #################################################

Pollutant <- c("PM25 (1ug/m^3)",
               "Ozone (1ppb)",
               "All Resp. HAPs (.1 RQ)",
               "Formaldehyde (.1 RQ)",                                          
               "Acetaldehyde (.1 RQ)",                                          
               "Acrolein (.1 RQ)",                                               
               "Naphthalene (.01 RQ)",
               "Diesel PM (.01 RQ)")

MRR <- c(estimates.pm$MRR[2],
         estimates.ozone$MRR[2],
         estimates.nata$MRR[2],
         estimates.Formaldheyde$MRR[2],
         estimates.ACETALDEHYDE$MRR[2],
         estimates.ACROLEIN$MRR[2],
         estimates.NAPHTHALENE.100x$MRR[2],
         estimates.diesel.100x$MRR[2])

CIH <- c(exp(estimates.pm$ci_high[2]),
         exp(estimates.ozone$ci_high[2]),
         exp(estimates.nata$ci_high[2]),
         exp(estimates.Formaldheyde$ci_high[2]),
         exp(estimates.ACETALDEHYDE$ci_high[2]),
         exp(estimates.ACROLEIN$ci_high[2]),
         exp(estimates.NAPHTHALENE.100x$ci_high[2]),
         exp(estimates.diesel.100x$ci_high[2]))

CIlow <- c(exp(estimates.pm$ci_low[2]),
           exp(estimates.ozone$ci_low[2]),
           exp(estimates.nata$ci_low[2]),
           exp(estimates.Formaldheyde$ci_low[2]),
           exp(estimates.ACETALDEHYDE$ci_low[2]),
           exp(estimates.ACROLEIN$ci_low[2]),
           exp(estimates.NAPHTHALENE.100x$ci_low[2]),
           exp(estimates.diesel.100x$ci_low[2]))

MRR.df.indv <- data.frame(Pollutant, MRR, CIlow, CIH)

# Single vs mutli effects chart - TABLE 2 ###################################
names(MRR.df.indv.10x)
names(MRR.df.indv.10x) <- c("var", "MRR", "MRR_Result_Single")

Pollutant <- c("PM25 (1ug/m^3)",
               "Ozone (1ppb)",
               "All Other HAPS (.1 RQ)",
               "Formaldehyde (.1 RQ)",                                          
               "Acetaldehyde (.1 RQ)",                                          
               "Acrolein (.1 RQ)",                                               
               "Naphthalene (.1 RQ)",
               "Diesel PM (.1 RQ)")

estimates.t5.10x$var[2:9] <- Pollutant

tab <- estimates.t5.10x %>% filter(var != "(Intercept)") %>% dplyr::select(var, MRR_Result)
indvtab <- MRR.df.indv.10x %>% dplyr::select(var, MRR_Result_Single)

mrrtab <- full_join(tab, indvtab)
# Table 2 csv
write.csv(mrrtab, "mrr_tble.csv")
