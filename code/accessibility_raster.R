# Script to generate accessibility to cities layer,
# using code/data from MAP's global accessibility surface project
# (Weiss et al. 2018)

# Code below adapted from Amelia Bertozzi-Villa's blogpost:
# https://medium.com/@abertozz/mapping-travel-times-with-malariaatlas-and-friction-surfaces-f4960f584f08

library(gdistance)
library(abind)
library(rje)
library(malariaAtlas)
library(rgeos)

IDN_ras = raster('data/admin/admin_IDN.tif')
indo.shp <- malariaAtlas::getShp(ISO = "IDN", admin_level = "admin1")
indo.buff = gBuffer(indo.shp, width = 1)
friction <- malariaAtlas::getRaster(
  surface = "A global friction surface enumerating land-based travel speed for a nominal year 2015",
  shp = indo.buff) # giving indo.buff a go to see if I can pick up some of the missing land pix

# generate transition matrix. (8 = no' adjacent pixels for each pixel)
Tr <- gdistance::transition(friction, function(x) 1/mean(x), 8) 
T.GC <- gdistance::geoCorrection(Tr) 

# Point locations - CITIES (population > 50,000) as reference points
# (At these locations, accessibility is zero, and all other travel times are to one of these points)
# An open source dataset of city locations:
# https://simplemaps.com/data/world-cities
cities <- read.csv("data/worldcities/worldcities.csv")

# keep cities with populations > 50000
point.locations <- cities[-which(cities$population < 50000), c("lng","lat","city")]
names(point.locations) <- c("X_COORD", "Y_COORD", "name")

# Keep only point coordinates within the shapefile bounds
coordinates(point.locations) <- ~ X_COORD + Y_COORD
proj4string(point.locations) <- proj4string(indo.shp)
overlap <- over(point.locations, indo.shp)
point.locations <- point.locations[!is.na(overlap$name_0),]

# cast to matrix of latlongs
point.coords <- as.matrix(point.locations@coords)

# accumulated cost surface
access.raster <- gdistance::accCost(T.GC, point.coords)

# match extent to extent of IDN admin raster
access.raster <- projectRaster(access.raster, IDN_ras)

writeRaster(access.raster,
            file = 'data/accessibility_raster_buffered',
            format = 'GTiff',
            overwrite = TRUE)









