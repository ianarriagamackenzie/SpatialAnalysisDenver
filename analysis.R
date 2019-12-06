## analysis spatial project
## fast food, groceries, affordable housing

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
library(sf)
library(spatstat)
library(smacpod)
library(spatialreg)
library(proxy)
library(geosphere)

display.brewer.all(colorblindFriendly = TRUE)

mapshape = readOGR(
  dsn = 'C:/Users/iansa/OneDrive/Documents/GitHub/spatial_analysis/SpatialAnalysisDenver/data/NBshapefile',
  layer = 'statistical_neighborhoods'
)
mapshapegg = tidy(mapshape)
mapshapesub = subset(mapshape, NBHD_NAME != 'DIA')
mapshapegg2 = tidy(mapshapesub)

AHP = read.csv("~/GitHub/spatial_analysis/SpatialAnalysisDenver/data/AffordableHousingLonLat.csv"); AHP$X = NULL
FFP = read.csv("~/GitHub/spatial_analysis/SpatialAnalysisDenver/data/FastFoodLonLat.csv"); FFP$X = NULL
GSP = read.csv("~/GitHub/spatial_analysis/SpatialAnalysisDenver/data/GroceryStoreLonLat.csv"); GSP$X = NULL
CountDat = read.csv("~/GitHub/spatial_analysis/SpatialAnalysisDenver/data/CountsFrame.csv"); CountDat$X = NULL
CountDat$NBID = NULL; CountDat = subset(CountDat, NBName != 'DIA')

## plots of data

ggplot(AHP) + 
  geom_point(data = AHP, mapping = aes(x = lon, y = lat), color = brewer.pal(3, 'Dark2')[1]) +
  geom_polygon(data = mapshapegg2, aes( x = long, y = lat, group = group), fill= NA, color="black") +
  theme_minimal() +
  labs(title = NULL, x = NULL, y = NULL) +
  coord_equal() + 
  theme(axis.text = element_blank())

ggplot(FFP) + 
  geom_point(data = FFP, mapping = aes(x = lon, y = lat), color = brewer.pal(3, 'Dark2')[2]) +
  geom_polygon(data = mapshapegg2, aes( x = long, y = lat, group = group), fill= NA, color="black") +
  theme_minimal() +
  labs(title = NULL, x = NULL, y = NULL) +
  coord_equal() + 
  theme(axis.text = element_blank())

ggplot(GSP) + 
  geom_point(data = GSP, mapping = aes(x = lon, y = lat), color = brewer.pal(3, 'Dark2')[3]) +
  geom_polygon(data = mapshapegg2, aes( x = long, y = lat, group = group), fill= NA, color="black") +
  theme_minimal() +
  labs(title = NULL, x = NULL, y = NULL) +
  coord_equal() + 
  theme(axis.text = element_blank())

#logrr tests

AHrr = AHP; AHrr$marks = rep('AH', times = 223)
FFrr = FFP; FFrr$marks = rep('FF', times = 710)
GSrr = GSP; GSrr$marks = rep('GS', times = 134)

AHFFrr = rbind(AHrr, FFrr)
AHFFppp = as.ppp(AHFFrr, W = mapshapesub)
AHFFpr = logrr(AHFFppp, case = 2, nsim = 499, level = 0.95)
plot(AHFFpr)

GSFFrr = rbind(GSrr, FFrr)
GSFFppp = as.ppp(GSFFrr, W = mapshapesub)
GSFFpr = logrr(GSFFppp, case = 2, nsim = 499, level = 0.95)
plot(GSFFpr)

AHGSrr = rbind(AHrr, GSrr)
AHGSppp = as.ppp(AHGSrr, W = mapshapesub)
AHGSpr = logrr(AHGSppp, case = 2, nsim = 499, level = 0.95)
plot(AHGSpr)

summary(m1 <- glm(AHcount ~ FFcount + GScount, family = "poisson", data = CountDat))

summary(m1 <- glm(GScount ~ AHcount, family = "poisson", data = CountDat))
summary(m1 <- glm(FFcount ~ AHcount, family = "poisson", data = CountDat))
summary(m1 <- glm(AHcount ~ FFcount, family = "poisson", data = CountDat))


distmat = dist(AHP, GSP)
distmat[1,]

pointframe = data.frame(lat = NULL,
                        lon = NULL)

i = 47

for (i in (1:223)){
  test = NULL
  test = distHaversine(AHP[i,], GSP)
  
  if (min(test) > 1609){
    pointframe = rbind(pointframe, AHP[i,])
  }
}

ggplot(pointframe) + 
  geom_point(data = pointframe, mapping = aes(x = lon, y = lat), color = brewer.pal(4, 'Dark2')[4]) +
  geom_polygon(data = mapshapegg2, aes( x = long, y = lat, group = group), fill= NA, color="black") +
  theme_minimal() +
  labs(title = NULL, x = NULL, y = NULL) +
  coord_equal() + 
  theme(axis.text = element_blank())
