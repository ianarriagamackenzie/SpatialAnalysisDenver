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


wqdat = read.csv("~/GitHub/spatial_analysis/SpatialAnalysisDenver/data/waterquality2018update_csv.csv")
toxdat = read.csv("~/GitHub/spatial_analysis/SpatialAnalysisDenver/data/tri_2017_co.csv")
ahpdat = read.csv("~/GitHub/spatial_analysis/SpatialAnalysisDenver/data/AffordableHousingProjects_CARH_20180530 - AffordableHousingProjects_CARH_20180530.csv")
fsdat = read.csv("~/GitHub/spatial_analysis/SpatialAnalysisDenver/data/food_stores.csv")

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

dmapsat %>%
  ggmap() +
  geom_point(data = fsdat2, mapping = aes(x = POINT_X, y = POINT_Y),
             color = brewer.pal(n = 8, name = 'Set2')[2]) +
  geom_point(data = ahpdat2, mapping = aes(x = longitude, y = latitude),
             color = brewer.pal(n = 8, name = 'Set2')[3])

dmap %>%
  ggmap() +
  geom_point(data = wqdat, mapping = aes(x = longitude, y = latitude),
             color = brewer.pal(n = 8, name = 'Set2')[2]) +
  geom_point(data = toxdat2, mapping = aes(x = X13..LONGITUDE, y = X12..LATITUDE),
             color = brewer.pal(n = 8, name = 'Set2')[6]) + 
  geom_point(data = ahpdat2, mapping = aes(x = longitude, y = latitude),
             color = brewer.pal(n = 8, name = 'Set2')[3])
