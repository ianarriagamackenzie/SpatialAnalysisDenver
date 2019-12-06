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
library(spatstat)
library(smacpod)

mapshape = readOGR(
  dsn = 'C:/Users/iansa/OneDrive/Documents/GitHub/spatial_analysis/SpatialAnalysisDenver/data/NBshapefile',
  layer = 'statistical_neighborhoods'
)
mapshapegg = tidy(mapshape)

mapshapesub = subset(mapshape, NBHD_NAME != 'DIA')
mapshapegg2 = tidy(mapshapesub)


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

write.csv(gssel2, file = 'GroceryStoreLonLat.csv')


coordinates(gsdat2) = ~longitude+latitude
proj4string(gsdat2) <- CRS(proj4string(mapshape))
gsselful = gsdat2[mapshape]


ffdat = read.csv("~/GitHub/spatial_analysis/SpatialAnalysisDenver/data/15783_1.csv")
ffdat2 = ffdat %>% 
  dplyr::select(isClosed, latitude, longitude, name) %>% 
  filter(!(isClosed %in% 'true')) %>% 
  dplyr::select(name, latitude, longitude) %>% 
  filter(!(is.na(latitude))) %>% 
  filter(!(is.na(longitude)))

ffpoint = data.frame(lon = ffdat2$longitude,
                     lat = ffdat2$latitude)

coordinates(ffpoint) = ~lon+lat
proj4string(ffpoint) <- CRS(proj4string(mapshape))
ffsel = ffpoint[mapshape]
ffsel2 = data.frame(lon = ffsel$lon,
                    lat = ffsel$lat)

write.csv(ffsel2, file = 'FastFoodLonLat.csv')

ahpdat = read.csv("~/GitHub/spatial_analysis/SpatialAnalysisDenver/data/AffordableHousingProjects_CARH_20180530 - AffordableHousingProjects_CARH_20180530.csv")
ahpdat2 = ahpdat %>%
  dplyr::select(longitude, latitude)
names(ahpdat2) = c('lon', 'lat')
ahpdat2$lon = as.numeric(as.character(ahpdat2$lon))
ahpdat2$lat = as.numeric(as.character(ahpdat2$lat))
ahpdat3 = ahpdat2 %>% 
  filter(!(is.na(lat))) %>% 
  filter(!(is.na(lon)))
coordinates(ahpdat3) = ~lon+lat
proj4string(ahpdat3) <- CRS(proj4string(mapshape))
ahsel = ahpdat3[mapshape]
ahsel2 = data.frame(lon = ahsel$lon,
                    lat = ahsel$lat)

write.csv(ahsel2, file = 'AffordableHousingLonLat.csv')

dmap = ggmap(get_googlemap(center = c(mean(ffdat2$longitude), mean(ffdat2$latitude)),
                           zoom = 11))

dmap +
  geom_point(data = ffdat2, mapping = aes(x = longitude, y = latitude, color = 'red')) +
  geom_point(data = gssel, mapping = aes(x = longitude, y = latitude, color = 'blue'))

ggplot(gsdat2) + 
  geom_point(data = gsdat2, mapping = aes(x = longitude, y = latitude, color = 'Grocery')) +
  geom_polygon(data = mapshapegg, aes( x = long, y = lat, group = group), fill= NA, color="black") +
  coord_equal()

ggplot(fssel2) + 
  geom_point(data = fssel2, mapping = aes(x = lon, y = lat, color = 'Grocery')) +
  geom_polygon(data = mapshapegg, aes( x = long, y = lat, group = group), fill= NA, color="black") +
  coord_equal()

ahcount = poly.counts(ahsel, mapshape)
ffcount = poly.counts(ffsel, mapshape)
gscount = poly.counts(gssel, mapshape)

countframe = data.frame(NBID = c(1:78),
                        ahcount = ahcount,
                        ffcount = ffcount,
                        gscount = gscount)

nbframe = data.frame(NBID = mapshape$NBHD_ID,
                     NBName = mapshape$NBHD_NAME)

countfinal = merge(countframe, nbframe, by = 'NBID')

write.csv(countfinal, file = 'CountsFrame.csv')


over(mapshape, gssel)
poly.counts(gssel, mapshape)

popincdat <- read.csv("~/GitHub/spatial_analysis/SpatialAnalysisDenver/data/american_community_survey_nbrhd_2010_2014.csv")
pidat = popincdat %>%
  dplyr::select(NBHD_NAME, TTL_POPULATION_ALL, MED_HH_INCOME)
names(pidat) = c('NBName', 'Pop', 'MedHHInc')

merge2 = merge(CountDat, pidat, by = 'NBName')

coordinates(AHP) = ~lon+lat
proj4string(AHP) <- CRS(proj4string(mapshape))

res = over(AHP, mapshape)
dframe = table(res$NBHD_NAME)

CountDat$AH2 = dframe

coordinates(FFP) = ~lon+lat
proj4string(FFP) <- CRS(proj4string(mapshape))

res = over(FFP, mapshape)
dframe = table(res$NBHD_NAME)

CountDat$FF2 = dframe

coordinates(GSP) = ~lon+lat
proj4string(GSP) <- CRS(proj4string(mapshape))

res = over(GSP, mapshape)
dframe = table(res$NBHD_NAME)

CountDat$GS2 = dframe

fspoint = fsdat2 %>%
  dplyr::select(POINT_X, POINT_Y)
names(fspoint) = c('lon', 'lat')


coordinates(fspoint) = ~lon+lat
proj4string(fspoint) <- CRS(proj4string(mapshape))
fssel = fspoint[mapshape]
fssel2 = data.frame(lon = fssel$lon,
                    lat = fssel$lat)

write.csv(fssel2, file = 'GroceryStoreLonLat.csv')

res = over(fssel, mapshape)
dframe = table(res$NBHD_NAME)

CountDat$GS3 = dframe

counfinal2= CountDat %>% 
  dplyr::select(NBName, NBID, Pop, MedHHInc, AH2, FF2, GS3)
names(counfinal2) = c('NBName', 'NBID', 'Pop', 'MedHHInc', 'AHcount', 'FFcount', 'GScount')

write.csv(counfinal2, file = 'CountsFrame.csv')

coordinates(FFP) = ~lon+lat
proj4string(FFP) <- CRS(proj4string(mapshapesub))
FFsel = FFP[mapshapesub]
FFsel2 = data.frame(lon = FFsel$lon,
                    lat = FFsel$lat)

write.csv(FFsel2, file = 'FastFoodLonLat.csv')
