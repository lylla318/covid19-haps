# Generate tables for the manuscript.

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



# Table 1: Summarize single and multi pollution model output.

# Create multi-pollutant MRR dataframe.
Pollutant <- c("All Other HAPS (.1 RQ)",
               "PM25 (1ug/m^3)",
               "Ozone (1ppb)",
               "Formaldehyde (.1 RQ)",                                          
               "Acetaldehyde (.1 RQ)",                                          
               "Acrolein (.1 RQ)",                                               
               "Naphthalene (.1 RQ)",
               "Diesel PM (.1 RQ)")

MRR1 <- exp(fixed(glmm.zinb.off.d.t5.10 )$dist[2,1])
MRR2 <- exp(fixed(glmm.zinb.off.d.t5.10 )$dist[3,1])
MRR3 <- exp(fixed(glmm.zinb.off.d.t5.10 )$dist[4,1])
MRR4 <- exp(fixed(glmm.zinb.off.d.t5.10 )$dist[5,1])
MRR5 <- exp(fixed(glmm.zinb.off.d.t5.10 )$dist[6,1])
MRR6 <- exp(fixed(glmm.zinb.off.d.t5.10 )$dist[7,1])
MRR7 <- exp(fixed(glmm.zinb.off.d.t5.10 )$dist[8,1])
MRR9 <- exp(fixed(glmm.zinb.off.d.t5.10 )$dist[9,1])
MRR <- c(MRR1,
         MRR2,
         MRR3,
         MRR4,
         MRR5,
         MRR6,
         MRR7,
         MRR9)
CIlow1 <- exp(fixed(glmm.zinb.off.d.t5.10 )$dist[2,1] - 1.96*fixed(glmm.zinb.off.d.t5.10 )$dist[2,2])
CIlow2 <- exp(fixed(glmm.zinb.off.d.t5.10 )$dist[3,1] - 1.96*fixed(glmm.zinb.off.d.t5.10 )$dist[3,2])
CIlow3 <- exp(fixed(glmm.zinb.off.d.t5.10 )$dist[4,1] - 1.96*fixed(glmm.zinb.off.d.t5.10 )$dist[4,2])
CIlow4 <- exp(fixed(glmm.zinb.off.d.t5.10 )$dist[5,1] - 1.96*fixed(glmm.zinb.off.d.t5.10 )$dist[5,2])
CIlow5 <- exp(fixed(glmm.zinb.off.d.t5.10 )$dist[6,1] - 1.96*fixed(glmm.zinb.off.d.t5.10 )$dist[6,2])
CIlow6 <- exp(fixed(glmm.zinb.off.d.t5.10 )$dist[7,1] - 1.96*fixed(glmm.zinb.off.d.t5.10 )$dist[7,2])
CIlow7 <- exp(fixed(glmm.zinb.off.d.t5.10 )$dist[8,1] - 1.96*fixed(glmm.zinb.off.d.t5.10 )$dist[8,2])
CIlow9 <- exp(fixed(glmm.zinb.off.d.t5.10 )$dist[9,1] - 1.96*fixed(glmm.zinb.off.d.t5.10 )$dist[9,2])
CIlow <- c(CIlow1,
           CIlow2,
           CIlow3,
           CIlow4,
           CIlow5,
           CIlow6,
           CIlow7,
           CIlow9)
CIH1 <- exp(fixed(glmm.zinb.off.d.t5.10 )$dist[2,1] + 1.96*fixed(glmm.zinb.off.d.t5.10 )$dist[2,2])
CIH2 <- exp(fixed(glmm.zinb.off.d.t5.10 )$dist[3,1] + 1.96*fixed(glmm.zinb.off.d.t5.10 )$dist[3,2])
CIH3 <- exp(fixed(glmm.zinb.off.d.t5.10 )$dist[4,1] + 1.96*fixed(glmm.zinb.off.d.t5.10 )$dist[4,2])
CIH4 <- exp(fixed(glmm.zinb.off.d.t5.10 )$dist[5,1] + 1.96*fixed(glmm.zinb.off.d.t5.10 )$dist[5,2])
CIH5 <- exp(fixed(glmm.zinb.off.d.t5.10 )$dist[6,1] + 1.96*fixed(glmm.zinb.off.d.t5.10 )$dist[6,2])
CIH6 <- exp(fixed(glmm.zinb.off.d.t5.10 )$dist[7,1] + 1.96*fixed(glmm.zinb.off.d.t5.10 )$dist[7,2])
CIH7 <- exp(fixed(glmm.zinb.off.d.t5.10 )$dist[8,1] + 1.96*fixed(glmm.zinb.off.d.t5.10 )$dist[8,2])
CIH9 <- exp(fixed(glmm.zinb.off.d.t5.10 )$dist[9,1] + 1.96*fixed(glmm.zinb.off.d.t5.10 )$dist[9,2])
CIH <- c(CIH1,
         CIH2,
         CIH3,
         CIH4,
         CIH5,
         CIH6,
         CIH7,
         CIH9)
MRR.df.d.t5.10x <- data.frame(Pollutant, MRR, CIlow, CIH)

# Create single pollutant MRR data frame.
Pollutant <- c("PM25 (1ug/m^3)",
               "Ozone (1ppb)",
               "All Resp. HAPS (.1 RQ)",
               "Formaldehyde (.1 RQ)",                                          
               "Acetaldehyde (.1 RQ)",                                          
               "Acrolein (.1 RQ)",                                               
               "Naphthalene (.1 RQ)",
               "Diesel PM (.1 RQ)")


MRR3 <- exp(fixed(glmm.zinb.off.nata)$dist[2,1])
MRR1 <- exp(fixed(glmm.zinb.off.pm)$dist[2,1])
MRR2 <- exp(fixed(glmm.zinb.off.ozone)$dist[2,1])
MRR4 <- exp(fixed(glmm.zinb.off.FORMALDEHYDE)$dist[2,1])
MRR5 <- exp(fixed(glmm.zinb.off.ACETALDEHYDE)$dist[2,1])
MRR6 <- exp(fixed(glmm.zinb.off.ACROLEIN)$dist[2,1])
MRR7 <- exp(fixed(glmm.zinb.off.NAPHTHALENE.b)$dist[2,1])
MRR9 <- exp(fixed(glmm.zinb.off.diesel.b)$dist[2,1])

MRR <- c(MRR1,
         MRR2,
         MRR3,
         MRR4,
         MRR5,
         MRR6,
         MRR7,
         MRR9)

CIlow3 <- exp(fixed(glmm.zinb.off.nata)$dist[2,1] - 1.96*fixed(glmm.zinb.off.nata)$dist[2,2])
CIlow1 <- exp(fixed(glmm.zinb.off.pm)$dist[2,1] - 1.96*fixed(glmm.zinb.off.pm)$dist[2,2])
CIlow2 <- exp(fixed(glmm.zinb.off.ozone)$dist[2,1] - 1.96*fixed(glmm.zinb.off.ozone)$dist[2,2])
CIlow4 <- exp(fixed(glmm.zinb.off.FORMALDEHYDE)$dist[2,1] - 1.96*fixed(glmm.zinb.off.FORMALDEHYDE)$dist[2,2])
CIlow5 <- exp(fixed(glmm.zinb.off.ACETALDEHYDE)$dist[2,1] - 1.96*fixed(glmm.zinb.off.ACETALDEHYDE)$dist[2,2])
CIlow6 <- exp(fixed(glmm.zinb.off.ACROLEIN)$dist[2,1] - 1.96*fixed(glmm.zinb.off.ACROLEIN)$dist[2,2])
CIlow7 <- exp(fixed(glmm.zinb.off.NAPHTHALENE.b)$dist[2,1] - 1.96*fixed(glmm.zinb.off.NAPHTHALENE.b)$dist[2,2])
CIlow9 <- exp(fixed(glmm.zinb.off.diesel.b)$dist[2,1] - 1.96*fixed(glmm.zinb.off.diesel.b)$dist[2,2])
CIlow <- c(CIlow1,
           CIlow2,
           CIlow3,
           CIlow4,
           CIlow5,
           CIlow6,
           CIlow7,
           CIlow9)
CIH3 <- exp(fixed(glmm.zinb.off.nata)$dist[2,1] + 1.96*fixed(glmm.zinb.off.nata)$dist[2,2])
CIH1 <- exp(fixed(glmm.zinb.off.pm)$dist[2,1] + 1.96*fixed(glmm.zinb.off.pm)$dist[2,2])
CIH2 <- exp(fixed(glmm.zinb.off.ozone)$dist[2,1] + 1.96*fixed(glmm.zinb.off.ozone)$dist[2,2])
CIH4 <- exp(fixed(glmm.zinb.off.FORMALDEHYDE)$dist[2,1] + 1.96*fixed(glmm.zinb.off.FORMALDEHYDE)$dist[2,2])
CIH5 <- exp(fixed(glmm.zinb.off.ACETALDEHYDE)$dist[2,1] + 1.96*fixed(glmm.zinb.off.ACETALDEHYDE)$dist[2,2])
CIH6 <- exp(fixed(glmm.zinb.off.ACROLEIN)$dist[2,1] + 1.96*fixed(glmm.zinb.off.ACROLEIN)$dist[2,2])
CIH7 <- exp(fixed(glmm.zinb.off.NAPHTHALENE.b)$dist[2,1] + 1.96*fixed(glmm.zinb.off.NAPHTHALENE.b)$dist[2,2])
CIH9 <- exp(fixed(glmm.zinb.off.diesel.b)$dist[2,1] + 1.96*fixed(glmm.zinb.off.diesel.b)$dist[2,2])
CIH <- c(CIH1,
         CIH2,
         CIH3,
         CIH4,
         CIH5,
         CIH6,
         CIH7,
         CIH9)
MRR.df.indv.10x <- data.frame(Pollutant, MRR, CIlow, CIH)

# Build Table 1, Part 1 - Single Pollutant MRR. 
names(MRR.df.indv.10x)
names(MRR.df.d.t5.10x) <- c("Pollutant", "MRR.m", "CIlow.m", "CIH.m")

mrrtab <- full_join(MRR.df.indv.10x, MRR.df.d.t5.10x)

mrrtab <- mrrtab %>%
  mutate("Multi-Pollutant" = paste0(round(MRR.m,2), " (",
                                    round(CIlow.m,2), " to ",
                                    round(CIH.m,2), ")"),
         "Single-Pollutant" = paste0(round(MRR,2), " (",
                                     round(CIlow,2), " to ",
                                     round(CIH,2), ")"))

mrrtab <- mrrtab %>%
  select(Pollutant,`Multi-Pollutant`, `Single-Pollutant`)

write.csv(mrrtab, "mrr_tble.csv")

# Build Table 1, Part 2 - Multi Pollutant MRR.
summary(glmm.zinb.off.d.t5.10)
exp(glmm.zinb.off.d.t5.10$coefficients$fixed)

summary(glmm.zinb.off.d.t5)
vars <- names(glmm.zinb.off.d.t5.10x$coefficients$fixed)
coefs <- exp(glmm.zinb.off.d.t5.10x$coefficients$fixed)
exp(fixed(glmm.zinb.off.d.t5.10)$dist)
CIH1 <- exp(fixed(glmm.zinb.off.d.t5.10)$dist + 1.96*fixed(glmm.zinb.off.d.t5.10)$dist)

resdf <- data.frame(CIH1)
resdf$Estimate <- round(resdf$Estimate, 3)

vars <- data.frame(names(exp(glmm.zinb.off.d.t5.10$coefficients$fixed)))

summary(glmm.zinb.off.d.t5.10)
gettab <- function(pos) {
  
  MRR0 <- exp(fixed(glmm.zinb.off.d.t5.10)$dist[pos,1])
  CIlow1 <- exp(fixed(glmm.zinb.off.d.t5.10)$dist[pos,1] - 1.96*fixed(glmm.zinb.off.d.t5.10)$dist[pos,2])
  CIH1 <- exp(fixed(glmm.zinb.off.d.t5.10)$dist[pos,1] + 1.96*fixed(glmm.zinb.off.d.t5.10)$dist[pos,2])
  Pval <- fixed(glmm.zinb.off.d.t5.10)$dist[pos,3]

  dat <- data.frame(MRR0, CIlow1, CIH1, Pval)
  return(dat)
}

varlist <- lapply(1:nrow(vars), function (x)  gettab(x))
varlist1 <- varlist[[1]]
for (i in 2:length(varlist)) varlist1 <- rbind(varlist1, varlist [[i]])

tableVar <- cbind(vars, varlist1)

tableVar2 <- tableVar %>%
  transmute(Variable = names.exp.glmm.zinb.off.d.t5.10.coefficients.fixed..,
              MRR = round(MRR0,2),
         "95% Confidence Interval" = paste0("(",
                                    round(CIlow1,2), " to ",
                                    round(CIH1,2), ")"),
         "P-value" = round(Pval, 2))

# Supplementary Table 1: Create a descriptive statistics table. 
pol_county_covid_tab <- pol_county_covid_mod %>% 
  transmute("Mean of block group ozone summer seasonal avg. of daily maximum 8-hour concentration in air in parts per billion" = OZONE_mean_2016,
            "Percent of people in a county living in linguistically isolated households 2016" = LINGISO_per_2016_county*100,
            "Covid-19 deaths up-to April 20th" = cov_deaths,
            "NATA respiratory hazard index 2014" = nataRespHaz_10x/10,
            "FORMALDEHYDE NATA respiratory hazard quotient 2014" = FORMALDEHYDE,
            "ACETALDEHYDE NATA respiratory hazard quotient 2014" = 	ACETALDEHYDE,
            "ACROLEIN	NATA respiratory hazard quotient 2014" = ACROLEIN,
            "NAPHTHALENE NATA respiratory hazard quotient 2014" = NAPHTHALENE,
            "Diesel PM NATA respiratory hazard quotient 2014" = diesel,
            "Average PM2.5 concentration 2000-2012" = pm25_chr,
            "Average median home value 2012-2018" = median_house_value_mean_12to18,
            "Average percentage of individuals with below high school education value 2012-2018" = Under_HS_Per_mean_12to18*100,
            "Average percentage of individuals below education value 2012-2018" = Poverty_Per_mean_12to18*100,
            "Average percent black 2012-2018" = Black_Per_mean_12to18*100,
            "Average percent hispanic 2012-2018" = Hispanic_Per_mean_12to18*100,
            "Average percent over 64 years old 2012-2018" = Over64_Per_mean_12to18*100,
            "Average population density population/sq mile 2012-2018" = popdensity_persqmile,
            "Average population smoking rate 2010-2018" = meanSmokeRate*100,
            "Total county hospital beds 2019" = countyHosipitalBeds,
            "County monthly average daily maximum temperature 2011 celsius" = max_temp_11,
            "County monthly average daily minimum temperature 2011 celsius" = min_temp_11,
            "Average percentage of adults that report a BMI of 30 or more 2004-2016" = obesity*100,
            "Average percentage of adults aged 20 and over reporting no leisure-time physical activity 2004-2016" = active*100,
            "Average hospital discharge percentage for ambulatory care-sensitive conditions fee-forservice Medicare enrollees 2012-2017" = prevent_hos/1000,
            "Total Covid-19 tests (State level)" = totaltests,
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

