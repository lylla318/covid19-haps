library(plotly)
library(rjson)

# Bar chart of the top 7 HAPS by source. 
names(usa1)
usa <- NataRespHazByChem[1,] %>% select(8:50) %>% gather("pollutant", "HQ14")  
names(pol_county_covid)
pol_county_covid <- pol_county_covid %>% mutate(AllbutDeisel = rowSums(select(.,79:94,96:121)))
pol_county_covid <- pol_county_covid %>% mutate(Allbut_top6 = rowSums(select(.,79:83, 86:91, 93:99, 101:109,111:121)))

# Pie chart with "All Others" category 
usa1 <- usa %>% mutate(rank = dense_rank(-HQ14)) %>%
  mutate(HAP = ifelse(rank > 6, "ALL OTHERS", pollutant)) %>%
  group_by(HAP) %>% summarise(HQ14 = sum(HQ14)) %>% ungroup()

hap_pie <- plot_ly(usa1, labels = ~HAP, values = ~HQ14, type = 'pie')
hap_pie  <- hap_pie %>% layout(title = 'United States Average Respiratory Hazard Quotient Contribution<br>by Hazardous Air Pollutant 2014',
                      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
hap_pie 

# Graph the single MRR estimates - must load table in Tables.R first.

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


# Map NATA 
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

# Map PM2.5 

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

# Map ozone 

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

# Map COVID-19 deaths
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


# Generate list of counties with the highest death per capita and highest HAP respiratory hazard quotient

ranker <- pol_county_covid %>% mutate(deathpercap_rank = dense_rank(-deathpercap),
                                      HQrank = dense_rank(-nataRespHaz)) %>% filter(deathpercap_rank < 100,
                                                                                    HQrank < 100) %>%
          select(State, County, GEOID, deathpercap, deathpercap_rank, nataRespHaz, HQrank)





