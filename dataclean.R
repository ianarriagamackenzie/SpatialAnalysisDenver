## data clean and merge
# spatial food deserts

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
library(tidyr)
library(gridExtra)
library(GISTools)
library(sp)

mapshape = readOGR(
  dsn = 'C:/Users/iansa/OneDrive/Documents/GitHub/spatial_analysis/SpatialAnalysisDenver/data/NBshapefile',
  layer = 'statistical_neighborhoods'
)

mapshapegg = tidy(mapshape)


gsdat = read.csv("~/GitHub/spatial_analysis/SpatialAnalysisDenver/data/15782_1.csv")
gsdat2 = gsdat %>% 
  dplyr::select(isClosed, latitude, longitude, name) %>% 
  filter(!(isClosed %in% 'true')) %>% 
  dplyr::select(name, latitude, longitude) %>% 
  filter(!(is.na(latitude))) %>% 
  filter(!(is.na(longitude)))

gspoints = data.frame(lon = gsdat2$longitude,
                      lat = gsdat2$latitude)
coordinates(gspoints) = ~lon+lat
proj4string(gspoints) <- CRS(proj4string(mapshape))
gssel = gspoints[mapshape]
gssel2 = data.frame(lon = gssel$lon,
                    lat = gssel$lat)


coordinates(gsdat2) = ~longitude+latitude
proj4string(gsdat2) <- CRS(proj4string(mapshape))


ffdat = read.csv("~/GitHub/spatial_analysis/SpatialAnalysisDenver/data/15783_1.csv")
ffdat2 = ffdat %>% 
  dplyr::select(isClosed, latitude, longitude, name) %>% 
  filter(!(isClosed %in% 'true')) %>% 
  dplyr::select(name, latitude, longitude) %>% 
  filter(!(is.na(latitude))) %>% 
  filter(!(is.na(longitude)))


dmap = ggmap(get_googlemap(center = c(mean(ffdat2$longitude), mean(ffdat2$latitude)),
                           zoom = 11))

dmap +
  geom_point(data = ffdat2, mapping = aes(x = longitude, y = latitude, color = 'red')) +
  geom_point(data = gsdat2, mapping = aes(x = longitude, y = latitude, color = 'blue'))

ggplot(gsdat2) + 
  geom_point(data = gsdat2, mapping = aes(x = longitude, y = latitude, color = 'Grocery')) +
  geom_polygon(data = mapshapegg, aes( x = long, y = lat, group = group), fill= NA, color="black") +
  coord_equal()

ggplot(gssel2) + 
  geom_point(data = gssel2, mapping = aes(x = lon, y = lat, color = 'Grocery')) +
  geom_polygon(data = mapshapegg, aes( x = long, y = lat, group = group), fill= NA, color="black") +
  coord_equal()


over(mapshape, gssel)
poly.counts(gssel, mapshape)
