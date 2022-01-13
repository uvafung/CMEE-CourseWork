### GIS Practical 1 ###
### Nov 1 2021 ###

install.packages("raster")
install.packages("sf")
install.packages("sp")
install.packages("rgeos")
install.packages("rgdal")
install.packages("lwgeom")

require(rgdal)
require(raster)
require(sf)
require(sp)
require(units)


pop_dens <- data.frame(n_km2 = c(260, 67, 151, 4500, 133),
                        country = c('England', 'Scotland', 'Wales', "London", 'Northern Ireland'))
print(pop_dens)

# Create coordinates  for each country 
# - this creates a matrix of pairs of coordinates forming the edge of the polygon. 
# - note that they have to _close_: the first and last coordinate must be the same.
scotland <- rbind(c(-5, 58.6), c(-3, 58.6), c(-4, 57.6), 
                  c(-1.5, 57.6), c(-2, 55.8), c(-3, 55), 
                  c(-5, 55), c(-6, 56), c(-5, 58.6))
england <- rbind(c(-2,55.8),c(0.5, 52.8), c(1.6, 52.8), 
                 c(0.7, 50.7), c(-5.7,50), c(-2.7, 51.5), 
                 c(-3, 53.4),c(-3, 55), c(-2,55.8))
wales <- rbind(c(-2.5, 51.3), c(-5.3,51.8), c(-4.5, 53.4),
               c(-2.8, 53.4),  c(-2.5, 51.3))
ireland <- rbind(c(-10,51.5), c(-10, 54.2), c(-7.5, 55.3),
                 c(-5.9, 55.3), c(-5.9, 52.2), c(-10,51.5))

# Convert these coordinates into feature geometries
# - these are simple coordinate sets with no projection information
scotland <- st_polygon(list(scotland))
england <- st_polygon(list(england))
wales <- st_polygon(list(wales))
ireland <- st_polygon(list(ireland))

uk_eire <- st_sfc(wales, england, scotland, ireland, crs=4326)
plot(uk_eire, asp=1)


### Making vector points from a dataframe ###
uk_eire_capitals <- data.frame(long= c(-0.1, -3.2, -3.2, -6.0, -6.25),
                               lat=c(51.5, 51.5, 55.8, 54.6, 53.30),
                               name=c('London', 'Cardiff', 'Edinburgh', 'Belfast', 'Dublin'))

# Indicate which fields in the data frame contain the coordinates
uk_eire_capitals <- st_as_sf(uk_eire_capitals, coords=c('long','lat'), crs=4326)
print(uk_eire_capitals)


st_pauls <- st_point(x=c(-0.098056, 51.513611))
london <- st_buffer(st_pauls, 0.25)

england_no_london <- st_difference(england, london)

lengths(scotland)
lengths(england_no_london)

wales <- st_difference(wales, england)

ni_area <- st_polygon(list(cbind(x=c(-8.1, -6, -5, -6, -8.1), y=c(54.4, 56, 55, 54, 54.4))))

northern_ireland <- st_intersection(ireland, ni_area)
eire <- st_difference(ireland, ni_area)

# Combine the final geometries
uk_eire <- st_sfc(wales, england_no_london, scotland, london, northern_ireland, eire, crs=4326)
print(uk_eire)

uk_country <- st_union(uk_eire[-6])
print(uk_country)

par(mfrow=c(1, 2), mar=c(3,3,1,1))
plot(uk_eire, asp=1, col=rainbow(6))
plot(st_geometry(uk_eire_capitals), add=TRUE)
plot(uk_country, asp=1, col='lightblue')

### Vector data and attributes
uk_eire <- st_sf(name=c('Wales', 'England','Scotland', 'London', 
                        'Northern Ireland', 'Eire'),
                 geometry=uk_eire)

plot(uk_eire, asp=1)

uk_eire$capital <- c('Cardiff', 'London', 'Edinburgh', 
                     NA, 'Belfast','Dublin')
print(uk_eire)

### spatial attributes ###
uk_eire_centroids <- st_centroid(uk_eire)
st_coordinates(uk_eire_centroids)

uk_eire$area <- st_area(uk_eire)
# To calculate a 'length' of a polygon, you have to 
# convert it to a LINESTRING or a MULTILINESTRING
# Using MULTILINESTRING will automatically 
# include all perimeter of a polygon (including holes).
uk_eire$length <- st_length(st_cast(uk_eire, 'MULTILINESTRING'))
# Look at the result
print(uk_eire)
st_distance(uk_eire)


### Creating a raster ###
uk_raster_WGS84 <- raster(xmn=-11,  xmx=2,  ymn=49.5, ymx=59, 
                          res=0.5, crs="+init=EPSG:4326")
hasValues(uk_raster_WGS84)

values(uk_raster_WGS84) <- seq(length(uk_raster_WGS84))
print(uk_raster_WGS84)

plot(uk_raster_WGS84)
plot(st_geometry(uk_eire), add=TRUE, border='black', lwd=2, col='#FFFFFF44')
m <- matrix(c(1, 1, 3, 3,
              1, 2, 4, 3,
              5, 5, 7, 8,
              6, 6, 7, 7), ncol=4, byrow=TRUE)
square <- raster(m)
