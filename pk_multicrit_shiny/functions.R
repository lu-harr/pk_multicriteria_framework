###############################################################################
# Greedy site selection

greedy_select_mindist = function(objective_surf, 
                                 pointsdf, # data frame - not spatial
                                 radius=50000,
                                 npoints=10,
                                 obj_col=FALSE,
                                 maximising=TRUE){
  excluded_ras = objective_surf
  out_points = c()  # return row indices
  pointsdf$ind = 1:nrow(pointsdf)
  shadows = list()
  
  if (obj_col == FALSE){
    # option to supply objective, else extract directly from objective_surf raster
    pointsdf$obj = values(objective_surf)[!is.na(values(objective_surf))]
    obj_col = "obj"
  }
  
  for (i in 1:npoints){
    if (nrow(pointsdf) == 0){
      message(paste0("The entire study area is excluded! ", i-1, " sites selected!"))
      break
    }
    new_ind = order(pointsdf[,obj_col], decreasing=maximising)[1]
    out_points[i] = pointsdf[new_ind, "ind"] # save index for return
    # uses original getDistanceCatchment (which returns raster)
    shadow = getDistanceCatchmentMulticrit(pointsdf[new_ind, c("lon","lat")], radius, objective_surf)
    shadows = append(shadows, shadow$pol)
    values(excluded_ras)[which(!is.na(values(shadow$ras)))] = NA # apply constraint to raster
    pointsdf = pointsdf[!is.na(extract(excluded_ras, pointsdf[,c("lon","lat")])),]
  }
  return(list(out_points=out_points, 
              excluded_ras=excluded_ras,
              shadows=shadows))
}



###############################################################################
# functions for catchments

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
