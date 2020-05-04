## Hazardous Air Pollutant Exposure as a Contributing Factor to COVID-19 Mortality in the United States

This repository contains code and data to reproduce the analysis from our paper "Hazardous Air Pollutant Exposure as a Contributing Factor to COVID-19 Mortality in the United States."

### Code:

[`Processing.R`](https://github.com/lylla318/covid19-haps/blob/master/Processing.R) contains all code for pre-processing the data for the analysis. Users must download three additional datafiles and drop them in the `data` directory:

* [2014 NATA Respiratory Hazard Index by Source Group](https://www.epa.gov/sites/production/files/2018-08/nata2014v2_national_resphi_by_tract_srcgrp.xlsx)
* [2014 NATA Respiratory Hazard Index by Pollutant](https://www.epa.gov/sites/production/files/2018-08/nata2014v2_national_resphi_by_tract_poll.xlsx)
* [EPA Environmental Justice Screening and Mapping Tool](ftp://newftp.epa.gov/EJSCREEN/2019/EJSCREEN_2019_USPR.csv.zip)


[`Analysis.R`](https://github.com/lylla318/covid19-haps/blob/master/Analysis.R) contains the code to implement both single and multi-pollutant mixture models. 

[`Figures.R`](https://github.com/lylla318/covid19-haps/blob/master/Figures.R) contains code to reproduce the figures in the paper.

[`Tables.R`](https://github.com/lylla318/covid19-haps/blob/master/Tables.R) contains code to reproduce the tables in the paper.

### Data:

| Data Type  | Source | Description |
| ------------- | ------------- | ------------- |
| Outcome: COVID-19 Deaths  | [Johns Hopkins University the Center for Systems Science and Engineering (CSSE) Coronavirus Resource Center](https://coronavirus.jhu.edu/)  | County Level COVID-19 death and case counts up to and including April 23, 2020  |
| Exposure: Hazardous Air Pollutant Exposure Risk  | [United States Environmental Protection Agency, National Air Toxics Assessment (NATA) 2014](https://www.epa.gov/national-air-toxics-assessment)  | Modeled average county concentrations of about 180 air toxics from natural, point, and non-point sources weighted by noncancer respiratory hazard indices.  |
| Exposure: PM2.5 concentrations  | [CDC Wonder Environmental data processed by County Health Rankings & Roadmaps](https://www.countyhealthrankings.org/)  | Modeled average daily PM2.5 (ug/m3) in a county, mean of years 2012-2014  |
| Exposure: Ground Level Ozone concentrations  | [Modeled by EPA using the CMAQ model for EPA EJScreen](https://www.epa.gov/ejscreen/technical-information-about-ejscreen)  | The May–September (summer/ ozone season) average of daily-maximum 8-hour-average ozone concentrations, in parts per billion (ppb) 2016  |
| Control Variable: Socio-demographics | [United States Census American Community Survey](https://www.census.gov/programs-surveys/acs) [EPA EJSCREEN](https://www.epa.gov/ejscreen/technical-information-about-ejscreen)  | County sociodemographic variables averaged 2012-2019: * Percent identifying as Black * Percent identifying as Hispanic * Percent Male 64 years of age and older * Percent Female 64 years of age and older * Percent less than High School Education * Percent Owner Occupied Housing * Percent under Poverty * Median Income * Median Housing Value * Linguistic Isolation  |
| Control Variable: County Behavioral Health  | [County Health Rankings & Roadmaps](https://www.countyhealthrankings.org/) compiled from the Behavioral Risk Factor Surveillance System (BRFSS) and Medicare claims data  | Average adult smoking rate 2014, Average 2012-2018 percentage of adults age 20 and over reporting no leisure time physical activity, Average 2012-2018 Number of hospital stays for ambulatory-care sensitive conditions per 100,000 Medicare enrollees  |
| Control Variable: Health Care Characteristics | [Homeland Infrastructure Foundation-Level Data (HIFLD)](https://hifld-geoplatform.opendata.arcgis.com/)  | Hospital beds per county 2019  |
| Control Variable: Covid-19 Testing | [The COVID tracking project](https://covidtracking.com/)  | State level Covid-19 Testing data up to and including April 19, 2020  |
| Control Variable: Monthly Min and Max Temperatures | [CDC Wonder Database](https://wonder.cdc.gov/) compiled from the NASA North America Land Data Assimilation System (NLDAS) Daily Air Temperatures and Heat Index  | County maximum and minimum temperatures per year 2011  |

### Additonal Sources:

### Contact Us:

### Terms of Use:
