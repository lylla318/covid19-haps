# Generate figures for the manuscript.
library(plotly)
library(rjson)

# Figure 1: Maps

# NATA Map
url <- 'https://raw.githubusercontent.com/plotly/datasets/master/geojson-counties-fips.json'
counties1 <- rjson::fromJSON(file=url)
g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = TRUE,
  lakecolor = toRGB('white')
)
fig <- plot_ly()
fig <- fig %>% add_trace(
  type="choropleth",
  geojson=counties1,
  locations=pol_county_covid$GEOID,
  z=pol_county_covid$nataRespHaz,
  colorscale="Viridis",
  zmin=0,
  zmax=max(pol_county_covid$nataRespHaz),
  marker=list(line=list(
    width=0)
  )
)
fig <- fig %>% colorbar(title = "NATA Respiratory Hazard")
fig <- fig %>% layout(
  title = "2014 NATA Respiratory Hazard by County"
)

fig <- fig %>% layout(
  geo = g
)

fig

# PM2.5 Map
g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = TRUE,
  lakecolor = toRGB('white')
)
fig2 <- plot_ly()
fig2 <- fig2 %>% add_trace(
  type="choropleth",
  geojson=counties1,
  locations=pol_county_covid$GEOID,
  z=pol_county_covid$pm25_chr,
  colorscale="Viridis",
  zmin=0,
  zmax=max(pol_county_covid$pm25_chr, na.rm =T),
  marker=list(line=list(
    width=0)
  )
)
fig2 <- fig2 %>% colorbar(title = "Average PM2.5 Concentration")
fig2 <- fig2 %>% layout(
  title = "2000-2012 Average PM2.5 Concentration by County"
)

fig2 <- fig2 %>% layout(
  geo = g
)

fig2

# Ozone Map 

g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = TRUE,
  lakecolor = toRGB('white')
)
fig3 <- plot_ly()
fig3 <- fig3 %>% add_trace(
  type="choropleth",
  geojson=counties1,
  locations=pol_county_covid$GEOID,
  z=pol_county_covid$OZONE_mean_2016,
  colorscale="Viridis",
  zmin=min(pol_county_covid$OZONE_mean_2016, na.rm =T),
  zmax=max(pol_county_covid$OZONE_mean_2016, na.rm =T),
  marker=list(line=list(
    width=0)
  )
)
fig3 <- fig3 %>% colorbar(title = "Average Ozone Concentration")
fig3 <- fig3 %>% layout(
  title = "2016 Average Ozone Concentration by County"
)

fig3 <- fig3 %>% layout(
  geo = g
)

fig3

# COVID-19 Mortality Map
pol_county_covid_map <- pol_county_covid %>% mutate(cov_deaths_map = ifelse(cov_deaths == 0, .1, cov_deaths))
g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = TRUE,
  lakecolor = toRGB('white')
)
fig4 <- plot_ly()
fig4 <- fig4 %>% add_trace(
  type="choropleth",
  geojson=counties1,
  locations=pol_county_covid_map$GEOID,
  z=log10(pol_county_covid_map$cov_deaths_map),
  colorscale="Viridis",
  zmin=0,
  zmax=max(log10(pol_county_covid_map$cov_deaths_map), na.rm =T),
  marker=list(line=list(
    width=0)
  )
)
fig4 <- fig4 %>% colorbar(title = "Covid 19 Deaths Up-to April")
fig4 <- fig4 %>% layout(
  title = "Covid-19 Deaths/Population April, 19th 2020"
)

fig4 <- fig4 %>% layout(
  geo = g
)

fig4


# Figure 2: Graph of MRR and 95% confidence interval.

# Make the data frame.
summary(glmm.zinb.off.d.t5)
Pollutant <- c("All Other HAPS (.1 RQ)",
               "PM25 (1ug/m^3)",
               "Ozone (1ppb)",
               "Formaldehyde (.1 RQ)",                                          
               "Acetaldehyde (.1 RQ)",                                          
               "Acrolein (.1 RQ)",                                               
               "Naphthalene (.01 RQ)",
               "Diesel PM (.01 RQ)")

MRR1 <- exp(fixed(glmm.zinb.off.d.t5)$dist[2,1])
MRR2 <- exp(fixed(glmm.zinb.off.d.t5)$dist[3,1])
MRR3 <- exp(fixed(glmm.zinb.off.d.t5)$dist[4,1])
MRR4 <- exp(fixed(glmm.zinb.off.d.t5)$dist[5,1])
MRR5 <- exp(fixed(glmm.zinb.off.d.t5)$dist[6,1])
MRR6 <- exp(fixed(glmm.zinb.off.d.t5)$dist[7,1])
MRR7 <- exp(fixed(glmm.zinb.off.d.t5)$dist[8,1])
MRR9 <- exp(fixed(glmm.zinb.off.d.t5)$dist[10,1])
MRR <- c(MRR1,
         MRR2,
         MRR3,
         MRR4,
         MRR5,
         MRR6,
         MRR7,
         MRR9)
CIlow1 <- exp(fixed(glmm.zinb.off.d.t5)$dist[2,1] - 1.96*fixed(glmm.zinb.off.d.t5)$dist[2,2])
CIlow2 <- exp(fixed(glmm.zinb.off.d.t5)$dist[3,1] - 1.96*fixed(glmm.zinb.off.d.t5)$dist[3,2])
CIlow3 <- exp(fixed(glmm.zinb.off.d.t5)$dist[4,1] - 1.96*fixed(glmm.zinb.off.d.t5)$dist[4,2])
CIlow4 <- exp(fixed(glmm.zinb.off.d.t5)$dist[5,1] - 1.96*fixed(glmm.zinb.off.d.t5)$dist[5,2])
CIlow5 <- exp(fixed(glmm.zinb.off.d.t5)$dist[6,1] - 1.96*fixed(glmm.zinb.off.d.t5)$dist[6,2])
CIlow6 <- exp(fixed(glmm.zinb.off.d.t5)$dist[7,1] - 1.96*fixed(glmm.zinb.off.d.t5)$dist[7,2])
CIlow7 <- exp(fixed(glmm.zinb.off.d.t5)$dist[8,1] - 1.96*fixed(glmm.zinb.off.d.t5)$dist[8,2])
CIlow9 <- exp(fixed(glmm.zinb.off.d.t5)$dist[10,1] - 1.96*fixed(glmm.zinb.off.d.t5)$dist[10,2])
CIlow <- c(CIlow1,
           CIlow2,
           CIlow3,
           CIlow4,
           CIlow5,
           CIlow6,
           CIlow7,
           CIlow9)
CIH1 <- exp(fixed(glmm.zinb.off.d.t5)$dist[2,1] + 1.96*fixed(glmm.zinb.off.d.t5)$dist[2,2])
CIH2 <- exp(fixed(glmm.zinb.off.d.t5)$dist[3,1] + 1.96*fixed(glmm.zinb.off.d.t5)$dist[3,2])
CIH3 <- exp(fixed(glmm.zinb.off.d.t5)$dist[4,1] + 1.96*fixed(glmm.zinb.off.d.t5)$dist[4,2])
CIH4 <- exp(fixed(glmm.zinb.off.d.t5)$dist[5,1] + 1.96*fixed(glmm.zinb.off.d.t5)$dist[5,2])
CIH5 <- exp(fixed(glmm.zinb.off.d.t5)$dist[6,1] + 1.96*fixed(glmm.zinb.off.d.t5)$dist[6,2])
CIH6 <- exp(fixed(glmm.zinb.off.d.t5)$dist[7,1] + 1.96*fixed(glmm.zinb.off.d.t5)$dist[7,2])
CIH7 <- exp(fixed(glmm.zinb.off.d.t5)$dist[8,1] + 1.96*fixed(glmm.zinb.off.d.t5)$dist[8,2])
CIH9 <- exp(fixed(glmm.zinb.off.d.t5)$dist[10,1] + 1.96*fixed(glmm.zinb.off.d.t5)$dist[10,2])
CIH <- c(CIH1,
         CIH2,
         CIH3,
         CIH4,
         CIH5,
         CIH6,
         CIH7,
         CIH9)
MRR.df.d.t5 <- data.frame(Pollutant, MRR, CIlow, CIH)

Pollutant <- c("PM25 (1ug/m^3)",
               "Ozone (1ppb)",
               "All Resp. HAPs (.1 RQ)",
               "Formaldehyde (.1 RQ)",                                          
               "Acetaldehyde (.1 RQ)",                                          
               "Acrolein (.1 RQ)",                                               
               "Naphthalene (.01 RQ)",
               "Diesel PM (.01 RQ)")

MRR3 <- exp(fixed(glmm.zinb.off.nata)$dist[2,1])
MRR1 <- exp(fixed(glmm.zinb.off.pm)$dist[2,1])
MRR2 <- exp(fixed(glmm.zinb.off.ozone)$dist[2,1])
MRR4 <- exp(fixed(glmm.zinb.off.FORMALDEHYDE)$dist[2,1])
MRR5 <- exp(fixed(glmm.zinb.off.ACETALDEHYDE)$dist[2,1])
MRR6 <- exp(fixed(glmm.zinb.off.ACROLEIN)$dist[2,1])
MRR7 <- exp(fixed(glmm.zinb.off.NAPHTHALENE)$dist[2,1])
MRR9 <- exp(fixed(glmm.zinb.off.diesel)$dist[2,1])

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
CIlow7 <- exp(fixed(glmm.zinb.off.NAPHTHALENE)$dist[2,1] - 1.96*fixed(glmm.zinb.off.NAPHTHALENE)$dist[2,2])
CIlow9 <- exp(fixed(glmm.zinb.off.diesel)$dist[2,1] - 1.96*fixed(glmm.zinb.off.diesel)$dist[2,2])
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
CIH7 <- exp(fixed(glmm.zinb.off.NAPHTHALENE)$dist[2,1] + 1.96*fixed(glmm.zinb.off.NAPHTHALENE)$dist[2,2])
CIH9 <- exp(fixed(glmm.zinb.off.diesel)$dist[2,1] + 1.96*fixed(glmm.zinb.off.diesel)$dist[2,2])
CIH <- c(CIH1,
         CIH2,
         CIH3,
         CIH4,
         CIH5,
         CIH6,
         CIH7,
         CIH9)
MRR.df.indv <- data.frame(Pollutant, MRR, CIlow, CIH)

# Graph Figure 2
MRR.df.indv$Pollutant <- factor(MRR.df.indv$Pollutant, as.character(MRR.df.indv$Pollutant))
MRR.df.indv$order <- c(1,2,3,4,5,6,7,8)
MRR.df.indv$Pollutant <- reorder(MRR.df.indv$Pollutant, -MRR.df.indv$order)

ggplot(MRR.df.indv, aes(x=MRR, y=Pollutant, group=1)) +
  geom_point(alpha=1, size =3, show.legend = F) +
  geom_errorbar(width=.1, aes(xmin=CIlow, xmax=CIH), show.legend = F) +
  scale_color_manual(values=c("darkblue", "darkred", "darkgreen")) +
  geom_vline(xintercept=1, linetype="dashed", color = "black", size = .2) + 
  annotate("text", x = 1.9, y = 7.5, label = "Critera Pollutants") +
  annotate("text", x = 1.9, y = 6, label = "Combined HAPs") +
  geom_hline(yintercept=6.5, linetype="solid", color = "black", size = 1) + 
  annotate("text", x = 1.9, y = 4.5, label = "Each Top-5 HAP") +
  geom_hline(yintercept=5.5, linetype="solid", color = "black", size = 1) + 
  labs(x="Mortality Rate Ratios",y= "Pollutant",
       title="Mortality Rate Ratios by Pollutant - Modeled Individually",
       subtitle="Expected additional Covid-19 death rate per added pollution unit with 95% confidence intervals"
       ) + theme(legend.position = c(0.8, 0.8)) + 
  scale_y_discrete(labels=c("PM25 (1ug/m^3)" = bquote(' '~PM[2.5]~ '(1 '*mu~g/m^3~ ')'),
                            "Ozone (1ppb)" = "Ozone (1ppb)",
                            "All Resp. HAPs (.1 RQ)"= "All Resp. HAPs (.1 RQ)",
                            "Formaldehyde (.1 RQ)" = "Formaldehyde (.1 RQ)",
                            "Acetaldehyde (.1 RQ)"= "Acetaldehyde (.1 RQ)",
                            "Acrolein (.1 RQ)"= "Acrolein (.1 RQ)",
                            "Naphthalene (.01 RQ)"="Naphthalene (.01 RQ)",
                            "Diesel PM (.01 RQ)" = "Diesel PM (.01 RQ)"))


