## water quality proposal eda
## CLEANED VER
## ian arriaga mackenzie

library(dplyr)
library(tidyverse)
library(mapdata)
library(maps)
library(stringr)
library(viridis)
library(ggplot2)
library(ggmap)
library(RColorBrewer)

display.brewer.all(colorblindFriendly = TRUE)

ahpdat = read.csv("~/GitHub/spatial_analysis/SpatialAnalysisDenver/data/AffordableHousingProjects_CARH_20180530 - AffordableHousingProjects_CARH_20180530.csv")
fsdat = read.csv("~/GitHub/spatial_analysis/SpatialAnalysisDenver/data/food_stores.csv")
ffdat1 = read.csv("~/GitHub/spatial_analysis/SpatialAnalysisDenver/data/FastFoodRestaurants.csv")
ffdat2 = read.csv("~/GitHub/spatial_analysis/SpatialAnalysisDenver/data/Datafiniti_Fast_Food_Restaurants.csv")
ffdat3 = read.csv("~/GitHub/spatial_analysis/SpatialAnalysisDenver/data/Datafiniti_Fast_Food_Restaurants_May19.csv")

fsdat2 = fsdat %>%
  filter(CITY %in% 'Denver') %>% 
  filter(STORE_TYPE %in% c('Small Grocery Store', 'Supercenter', 'Superette',
                           'Supermarket', 'Warehouse Club Store', 'Dollar Store',
                           'Discount Merchandise Market')) %>% 
  select(STORE_NAME, POINT_X, POINT_Y) %>% 
  filter(!is.na(POINT_X)) %>% 
  filter(!is.na(POINT_Y))

ahpdat$longitude = as.numeric(as.character(ahpdat$longitude))
ahpdat$latitude = as.numeric(as.character(ahpdat$latitude))
ahpdat2 = ahpdat %>% 
  filter(city %in% 'Denver')
ahpdat3 = ahpdat2 %>% 
  select(latitude, longitude, programs_and_designations) %>% 
  filter(latitude < (max(fsdat2$POINT_Y) + 0.02)) %>% 
  filter(latitude > 39.7 | longitude < -104.85) %>% 
  filter(!is.na(longitude)) %>% 
  filter(!is.na(latitude)) %>% 
  filter(programs_and_designations != 'NULL')

ffdat1c = ffdat1 %>%
  select(latitude, longitude, name, province, city)
ffdat2c = ffdat2 %>%
  select(latitude, longitude, name, province, city)
ffdat3c = ffdat3 %>%
  select(latitude, longitude, name, province, city)

ffdatm = rbind(ffdat1c, ffdat2c, ffdat3c)
ffdatm2 = ffdatm %>% 
  filter(province %in% 'CO') %>% 
  filter(city %in% 'Denver') %>% 
  select(latitude, longitude, name) %>% 
  distinct() %>%
  filter(longitude > (min(fsdat2$POINT_X) - 0.01)) %>% 
  filter(latitude < (max(fsdat2$POINT_Y) + 0.02)) %>% 
  filter(!is.na(longitude)) %>% 
  filter(!is.na(latitude))

dmap = get_googlemap(center = c(mean(ahpdat3$longitude), mean(ahpdat3$latitude)),
                     zoom = 11)
dmapfar = get_googlemap(center = c(mean(ahpdat3$longitude), mean(ahpdat3$latitude)),
                     zoom = 10)
dmapzoom = get_googlemap(center = c(mean(ahpdat3$longitude), mean(ahpdat3$latitude)),
                     zoom = 12)
dmapsat = get_googlemap(center = c(mean(ahpdat3$longitude), mean(ahpdat3$latitude)),
                        zoom = 11, maptype = 'satellite')

dmap %>%
  ggmap() +
  geom_point(data = ahpdat3, mapping = aes(x = longitude, y = latitude),
             color = brewer.pal(n = 10, name = 'Paired')[10]) +
  geom_point(data = fsdat2, mapping = aes(x = POINT_X, y = POINT_Y),
             color = brewer.pal(n = 12, name = 'Paired')[12]) +
  geom_point(data = ffdatm2, mapping = aes(x = longitude, y = latitude),
             color = brewer.pal(n = 10, name = 'Paired')[6])

unique(fsdat2$STORE_NAME)
unique(ffdatm2$name)
unique(ahpdat3$programs_and_designations)
