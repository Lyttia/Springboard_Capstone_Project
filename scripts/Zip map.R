# Phoenix zip code crime maps

# Install & Load packages
library(tidyverse)
library(maptools)
library(ggplot2)
devtools::install_github("dkahle/ggmap", ref = "tidyup", dependencies = T)
library(ggmap)

# if you want to type data file path/name use: 
# area <- readShapePoly(".shp")

# if you want to choose data file from pc use: 
area <- readShapePoly(file.choose())

# set colors
library(RColorBrewer)
colors <- brewer.pal(9, "BuGn")

# set up PHX base map from Google Map Static API
# must obtain API key from Google, assign to project, and enable billing
register_google(key = hidden)

# store phx base map image
mapImage <- get_map(location = c(lon = -112.1, lat = 33.52), zoom = 11, 
                          maptype = "hybrid", source = "google")

area.points <- fortify(area)
  
PHX_zipmp <- ggmap(mapImage) +
  geom_polygon(aes(x = long, y = lat, group = group), data = area.points, 
               color = colors[9], fill = colors[6], alpha = 0.5) +
  labs(x = "Longitude", y = "Latitude")

PHX_zipmp

ggsave("PHXzips_shapemap.png")

# load zip coordinate data
zipcoord <- read_csv(file.choose())

# cleaning zipcode data file
zipcoord1 <- zipcoord %>% 
  select(Zipcode, City, State, Lat, Long) %>% 
  rename(zipcode = Zipcode)

View(zipcoord1)

# join zip coordinates to crime data
crimes4coord <- left_join(crimes4, zipcoord1) %>% 
  select(-zipname)

View(crimes4coord)

# not super helpful: all of the points in one zipcode were assigned to the same 
# coordinate, when jittered, crimes cover the entire city, with no clear distinctions
phxcrimemap_byzip <- ggmap(mapImage, extent ='device') +
  geom_jitter(aes(x = Long, y = Lat, colour = category, alpha = 0.), 
              data = crimes4coord, width = 0.5, height = 0.5) +  
  ggtitle('Crime in Phoenix')
phxcrimemap_byzip
ggsave("Crime Map by Zip.png")

# May be helpful to add alpha value, group categories, or plot violent/nonviolent

# Warning message:
# Removed 163346 rows containing missing values (geom_point).
# Check for NA or values outside of map range. can use: is.na(dataframe$variable)
# Can I add zip shape map as a layer to map above or vise versa 
# (plot crime data on top of zip shape map)? 

# broken apart by crime category
phxcrimemap_bycat <- ggmap(mapImage, extent='device') +
  geom_point(aes(x= Long, y= Lat, colour= category), data=crimes4coord) +
  scale_colour_discrete(guide='none') +
  facet_wrap(~ category) +
  ggtitle('Crime in Phoenix')
phxcrimemap_bycat
ggsave("Crime Map by Category.png")

# More helpful map would show the density of crime in each zipcode

# store contours variable
contours <- stat_density2d(
  aes(x = Long, y = Lat, fill = ..level.., alpha=..level..),
  size = 0.1, data = crimes4coord, n=200,
  geom = "polygon")

# Can I add jitter, since all points for each zipcode are codded
# to the same lat/long coordinate?
crimedensity_byzip <- ggmap(mapImage, extent='device') + 
  contours +
  scale_alpha_continuous(range=c(0.25,0.4), guide='none') +
  scale_fill_gradient('Crime\nDensity')+
  ggtitle('Crime Density in Phoenix') 

crimedensity_byzip
  ggsave("Crime Density by Zip.png")
# Warning message:
# Removed 11215 rows containing non-finite values (stat_density2d).
# Check for NA or values outside of map range. 

citation("ggmap")
