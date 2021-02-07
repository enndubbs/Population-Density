#Setup ####

#I know this is excessive, it's every package I've used for the past six months.
#Don't judge, 2020 was rough for everyone.

library(tidycensus)
library(tidyverse)
library(skmeans)
library(stringr)
library(httr)
library(readr)
library(sf)
library(maptools)
library(GGally)
library(cluster)
library(factoextra)
library(tmap)
library(cartogram)

census_api_key("Your API Key Here",install=TRUE,overwrite=TRUE)

fips<-fips_codes
states<-unique(fips_codes$state)[1:51]

#Census Planning Database, Tract-level
#https://api.census.gov/data/2019/pdb.html

Planning<-read_csv("Data\\pdb2019trv6_us.csv")

view(colnames(Planning))

#Shift_Geo is very handy but unfortunately doesn't work for any geographies except County and State
GeometryPop_Grab <- function(Geogr, Years = 2019) {
  if(Geogr == "state" | Geogr == "county") {
    map_df(Years, ~get_acs(year = .x, geography = Geogr, variables = "B01001_001", geometry = TRUE, shift_geo = TRUE) %>% 
             mutate(Year = .x)
    )}
  else {
    map_df(Years, ~get_acs(year = .x, geography = Geogr, variables = "B01001_001", geometry = TRUE, shift_geo = FALSE) %>% 
             mutate(Year = .x))
  }
}

National <- c("state", "place", "county", "metropolitan statistical area/micropolitan statistical area", "zcta")
By_State <- c("tract", "blockgroup", "block")

#I couldn't find a better way to grab all tract-level data from multiple years and variables a time, so I wrote this function
ACS_Grab <- function(Vars, Geogr, Years = 2019) {
  if(Geogr %in% National) {
    map_df(Years, function(Yrs) {
      get_acs(year = Yrs, geography = Geogr, variables = Vars, geometry = FALSE) %>% 
        mutate(Year = Yrs,
               High_Error = ((moe/1.645)/estimate)>.2)
    } 
    )}
  else if (Geogr %in% By_State) {
    map_df(Years, function(Yrs) {
      map_df(states, ~get_acs(year = Yrs, geography = Geogr, state = .x, variables = Vars, geometry = FALSE)) %>% 
        mutate(Year = Yrs,
               High_Error = ((moe/1.645)/estimate)>.2)}
    )
  }
}

#By County ####

Population_By_County <- GeometryPop_Grab('county')

write_csv(Population_By_County, "Data\\Population_By_County.csv")

County_Categories <- Population_By_County %>% 
  arrange(desc(estimate)) %>% 
  mutate(Proportion = cumsum(estimate)/sum(estimate),
         Categories_2 = if_else(Proportion < .5, "1", "2"))

County_Cat_5050<-data.frame(County_Categories) %>% 
  group_by(Categories_2) %>% 
  summarize(Population = sum(estimate))

#50-50

County_Categories %>% 
  inner_join(County_Cat_5050) %>% 
  ggplot(aes(fill = Categories_2)) +
  geom_sf() +
  coord_sf() +
  facet_wrap(~Population,
             labeller = label_both) +
  theme(strip.text.x = element_text(size = 60),
        legend.position = "none",
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())

#Single County

County_Cat_Single<-data.frame(County_Categories) %>% 
  filter(Proportion<=County_Categories$Proportion[1] | Proportion>=(1-County_Categories$Proportion[1])) %>% 
  group_by(Categories_2) %>% 
  summarize(Population = sum(estimate))

County_Categories %>% 
  filter(Proportion<=County_Categories$Proportion[1] | Proportion>=(1-County_Categories$Proportion[1])) %>% 
  inner_join(County_Cat_Single) %>% 
  ggplot(aes(fill = Categories_2)) +
  geom_sf() +
  coord_sf() +
  facet_wrap(~Population,
             labeller = label_both) +
  theme(strip.text.x = element_text(size = 60),
        legend.position = "none",
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())

#By Tract ####

Population_By_Tract<-map_df(states, ~get_acs(year = 2019, geography = "tract", state = .x, variables = "B01001_001", geometry = TRUE))

write_csv(Population_By_Tract, "Data\\Population_By_Tract.csv")

Population_Density_By_Tract<-Population_By_Tract %>% 
  left_join(select(Planning,
                   GEOID = GIDTR, LAND_AREA)) %>% 
  mutate(Density = estimate/LAND_AREA) %>% 
  filter(Density!=Inf)

Tract_Categories <- Population_Density_By_Tract %>% 
  arrange(desc(Density)) %>% 
  mutate(Proportion = cumsum(estimate)/sum(estimate),
         Categories_2 = if_else(Proportion < .5, "1", "2"))

#50-50

Tract_Cat_5050<-data.frame(Tract_Categories) %>% 
  group_by(Categories_2) %>% 
  summarize(Population = sum(estimate))

Tract_Categories %>% 
  inner_join(Tract_Cat_5050) %>% 
  filter(str_detect(NAME, "Hawaii") == FALSE,
         str_detect(NAME, "Alaska") == FALSE) %>% 
  ggplot(aes(fill = Categories_2)) +
  geom_sf(color = NA) +
  coord_sf() +
  facet_wrap(~Population,
             labeller = label_both) +
  theme(strip.text.x = element_text(size = 60),
        legend.position = "none",
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())

#10 percent

Tract_Cat_10_Percent<-data.frame(Tract_Categories) %>% 
  filter(Proportion<=.1 | Proportion>=.9) %>% 
  group_by(Categories_2) %>% 
  summarize(Population = sum(estimate))

Tract_Categories %>% 
  inner_join(Tract_Cat_10_Percent) %>% 
  filter(Proportion<=.1 | Proportion>=.9) %>% 
  filter(str_detect(NAME, "Hawaii") == FALSE,
         str_detect(NAME, "Alaska") == FALSE,
         Proportion<=.1 | Proportion>=.9) %>% 
  ggplot(aes(fill = Categories_2)) +
  geom_sf(color = NA) +
  coord_sf() +
  facet_wrap(~Population,
             labeller = label_both) +
  theme(strip.text.x = element_text(size = 60),
        legend.position = "none",
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())

#1 percent

Tract_Cat_1_Percent<-data.frame(Tract_Categories) %>% 
  filter(Proportion<=.01 | Proportion>=.99) %>% 
  group_by(Categories_2) %>% 
  summarize(Population = sum(estimate))

OnePercent<-Tract_Categories %>% 
  inner_join(Tract_Cat_1_Percent) %>% 
  filter(str_detect(NAME, "Hawaii") == FALSE,
         str_detect(NAME, "Alaska") == FALSE,
         Proportion<=.01 | Proportion>=.99) %>% 
  ggplot(aes(fill = Categories_2)) +
  geom_sf(color = NA) +
  coord_sf() +
  facet_wrap(~Population,
             labeller = label_both) +
  theme(strip.text.x = element_text(size = 60),
        legend.position = "none",
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())