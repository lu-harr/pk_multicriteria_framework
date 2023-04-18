###############################################################################
# Distance catchment / minimum distance helpers

# Minimum distance constraint can be accomplished via getDistanceCatchment
getDistanceCatchment <- function(point, radius, mask){
  # returns a raster of a catchment area in the radius around the point provided
  # adapted from ENMTools::background.points.buffer
  point = as.numeric(point)
  point = matrix(point, nrow=1)
  x <- dismo::circles(point, d=radius, lonlat=TRUE)
  pol <-  gUnaryUnion(x@polygons)
  buffer.raster <- mask(mask, pol)
  return(buffer.raster)
}

getDistanceCatchmentMulticrit <- function(point, radius, 
                                          precis, uncert=raster(matrix(0))){
  # edit of getDistanceCatchment
  # this guy returns catchment polygon and some summary stats
  # (original version provides masked raster)
  # hardcoded for two objective surfaces !
  point = as.numeric(point)
  point = matrix(point, nrow=1)
  x <- dismo::circles(point, d=radius, lonlat=TRUE)
  pol <-  gUnaryUnion(x@polygons)
  
  if (length(getValues(uncert)) != 1){ # biojective case
    precis = trim(mask(precis, pol))
    uncert = trim(mask(uncert, pol))
    return(c(pol,
             mean(getValues(precis), na.rm=TRUE),
             sd(getValues(precis), na.rm=TRUE),
             mean(getValues(uncert), na.rm=TRUE),
             sd(getValues(uncert), na.rm=TRUE))) # include catch size
    
  } else { # uniobjective case: want the polygon and the raster
    return(list(pol=pol,
                ras=mask(precis, pol)))
  }
  
}