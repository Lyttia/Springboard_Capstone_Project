#Phoenix zip code map
library(tidyverse)
install.packages("maptools")
library(maptools)

# type file path/name
# area <- readShapePoly(".shp")

#choose file from pc
area <- readShapePoly(file.choose())

#set colors
library(RColorBrewer)
colors <- brewer.pal(9, "BuGn")

# set up base map
library(ggplot2)
if(!requireNamespace("devtools")) install.packages("devtools")
devtools::install_github("dkahle/ggmap", ref = "tidyup", dependencies = T)
library(ggmap)
register_google(key = "AIzaSyDyUl434XV34WQoc3sHq9cfUCjltdlAvuI")
mapImage <- get_map(location = c(lon = -112.1, lat = 33.52), zoom = 11, 
                          maptype = "hybrid", source = "google")

area.points <- fortify(area)
  
PHX_zipmp <- ggmap(mapImage) +
  geom_polygon(aes(x = long, y = lat, group = group), data = area.points, 
               color = colors[9], fill = colors[6], alpha = 0.5) +
  labs(x = "Longitude", y = "Latitude")

PHX_zipmp

zipcoord <- read_csv(file.choose())

zipcoord1 <- zipcoord %>% 
  select(Zipcode, City, State, Lat, Long) %>% 
  rename(zipcode = Zipcode)

zipcoord1

crimes4coord <- left_join(crimes4, zipcoord1) %>% 
  select(-zipname)

View(crimes4coord)

#not helpful: all of the points in one zipcode were assigned to the same coordinate
ggmap(mapImage, extent='device', legend="topleft") +
  geom_point(aes(x= Long, y= Lat, colour=category), data=crimes4coord) +  
  ggtitle('Crime in Phoenix')

# broken apart by crime category
ggmap(mapImage, extent='device') +
  geom_point(aes(x= Long, y= Lat, colour= category), data=crimes4coord) +
  scale_colour_discrete(guide='none') +
  facet_wrap(~ category) +
  ggtitle('Crime in Phoenix')

# More helpful map as I can see the density of crime in each zipcode
contours <- stat_density2d(
  aes(x = Long, y = Lat, fill = ..level.., alpha=..level..),
  size = 0.1, data = crimes4coord, n=200,
  geom = "polygon")

ggmap(mapImage, extent='device', legend="topleft") + contours +
  scale_alpha_continuous(range=c(0.25,0.4), guide='none') +
  scale_fill_gradient('Violent\nCrime\nDensity')+
  ggtitle('Crime in Phoenix')
