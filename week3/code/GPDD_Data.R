# Author: Uva Fung
# Date: Nov 5 2021
# Description: mapping the Global Population Dynamics Database (GPDD) on a world map

rm(list=ls())

load ("../data/GPDDFiltered.RData") # load data gpdd


require(maps)
require(ggplot2)
require(mapdata)

worldmap <- map_data("world") # gives a dataframe of points outlining the world

connect_world <- ggplot() +   # creates a ggplot that join up the points to form polygons 
  geom_polygon(data = worldmap, 
               aes(x = long, y = lat, group = group),
               fill = NA, colour = "black") +
  coord_fixed(1.3)   # fixed map ratio

connect_world # print world map out

dotted_world <- connect_world + 
  geom_point(data = gpdd, aes(x = long, y = lat), colour = "orange") # print world map with data points

print(dotted_world)

print("Script completes!")   # print to show that script is working
                        

# An analysis based on this data may be biased because most data are collected from North America and Europe. 
# Other geographic regions with different climates and biodiversity (eg. the tropics) are excluded.

  



