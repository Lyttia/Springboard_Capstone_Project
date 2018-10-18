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

