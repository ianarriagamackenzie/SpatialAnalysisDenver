## water quality proposal eda
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
library(rgdal)
library(broom)

display.brewer.all(colorblindFriendly = TRUE)

wqdat = read.csv("~/GitHub/spatial_analysis/SpatialAnalysisDenver/data/waterquality2018update_csv.csv")
toxdat = read.csv("~/GitHub/spatial_analysis/SpatialAnalysisDenver/data/tri_2017_co.csv")
ahpdat = read.csv("~/GitHub/spatial_analysis/SpatialAnalysisDenver/data/AffordableHousingProjects_CARH_20180530 - AffordableHousingProjects_CARH_20180530.csv")
fsdat = read.csv("~/GitHub/spatial_analysis/SpatialAnalysisDenver/data/food_stores.csv")
ffdat1 = read.csv("~/GitHub/spatial_analysis/SpatialAnalysisDenver/data/FastFoodRestaurants.csv")
ffdat2 = read.csv("~/GitHub/spatial_analysis/SpatialAnalysisDenver/data/Datafiniti_Fast_Food_Restaurants.csv")
ffdat3 = read.csv("~/GitHub/spatial_analysis/SpatialAnalysisDenver/data/Datafiniti_Fast_Food_Restaurants_May19.csv")

mapshape = readOGR(
  dsn = 'C:/Users/iansa/OneDrive/Documents/GitHub/spatial_analysis/SpatialAnalysisDenver/data/NBshapefile',
  layer = 'statistical_neighborhoods'
)


ffdat1c = ffdat1 %>%
  filter(province %in% 'CO') %>% 
  filter(city %in% 'Denver') %>% 
  select(latitude, longitude, name)
ffdat2c = ffdat2 %>%
  filter(province %in% 'CO') %>% 
  filter(city %in% 'Denver') %>% 
  select(latitude, longitude, name)
ffdat3c = ffdat3 %>%
  filter(province %in% 'CO') %>% 
  filter(city %in% 'Denver') %>%
  select(latitude, longitude, name)

ffdatm = rbind(ffdat1c, ffdat2c, ffdat3c)
ffdatm = ffdatm %>% 
  distinct() %>% 
  filter(longitude > (min(fsdat2$POINT_X) - 0.01))


toxdat2 = toxdat %>%
  filter(X12..LATITUDE > min(wqdat$latitude)) %>% 
  filter(X12..LATITUDE < max(wqdat$latitude)) %>% 
  filter(X13..LONGITUDE > min(wqdat$longitude)) %>% 
  filter(X13..LONGITUDE < max(wqdat$longitude))

plot(wqdat$latitude, wqdat$longitude)
plot(toxdat2$X12..LATITUDE, toxdat2$X13..LONGITUDE)

states <- map_data("state")

comap <- states %>%
  filter(region %in% 'colorado')

ahpdat2 = ahpdat %>% 
  filter(city %in% 'Denver')
ahpdat2$longitude = as.numeric(as.character(ahpdat2$longitude))
ahpdat2$latitude = as.numeric(as.character(ahpdat2$latitude))

fsdat2 = fsdat %>%
  filter(CITY %in% 'Denver') %>% 
  filter(STORE_TYPE %in% c('Small Grocery Store', 'Supercenter', 'Superette', 'Supermarket', 'Warehouse Club Store'))

dmap = get_googlemap(center = c(mean(wqdat$longitude), mean(wqdat$latitude)), zoom = 11)
dmapsat = get_googlemap(center = c(mean(wqdat$longitude), mean(wqdat$latitude)), zoom = 11, maptype = 'satellite')

dmap %>%
  ggmap() +
  geom_point(data = fsdat2, mapping = aes(x = POINT_X, y = POINT_Y),
             color = brewer.pal(n = 10, name = 'Paired')[2]) +
  geom_point(data = ahpdat2, mapping = aes(x = longitude, y = latitude),
             color = brewer.pal(n = 10, name = 'Paired')[8]) +
  geom_point(data = ffdatm, mapping = aes(x = longitude, y = latitude),
             color = brewer.pal(n = 10, name = 'Paired')[6])

dmap %>%
  ggmap() +
  geom_point(data = wqdat, mapping = aes(x = longitude, y = latitude),
             color = brewer.pal(n = 8, name = 'Set2')[2]) +
  geom_point(data = toxdat2, mapping = aes(x = X13..LONGITUDE, y = X12..LATITUDE),
             color = brewer.pal(n = 8, name = 'Set2')[6]) + 
  geom_point(data = ahpdat2, mapping = aes(x = longitude, y = latitude),
             color = brewer.pal(n = 8, name = 'Set2')[3])
