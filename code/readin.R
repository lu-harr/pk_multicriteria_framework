# Initial script:
#   - Reads in key functions
#   - Reads in rasters/pre-prepared outputs
#   - Should give enough to generate all of the figures in the main text
#   - Should NOT take long to run :')
library(viridisLite)
library(scales)
library(raster)
library(spdep) # moran
library(dismo) # dist catches
library(rgeos) # dist catches
library(rgdal) # spdf
library(dplyr) # setdiff
library(rgdal)

plotpath = "/output/"

source("code/get_distance_catchments.R")

###############################################################################
# Study area: Kalimantan and Sumatra only ...
idn_shp = shapefile("data/admin/admin2013_1.shp")
idn_shp = subset(idn_shp, idn_shp$COUNTRY_ID == "IDN")
IDN_ras <- raster("data/admin/admin_IDN.tif")
sumatera_shp = subset(idn_shp, idn_shp$NAME %in% c("SUMATERA UTARA",
                                                   "SUMATERA BARAT",
                                                   "SUMATERA SELATAN",
                                                   "RIAU",
                                                   "JAMBI","BENGKULU",
                                                   "LAMPUNG",
                                                   "NANGGROE ACEH DARUSALAM"))  
# (left off some island provinces)
kalimantan_shp = subset(idn_shp, idn_shp$NAME %in% c("KALIMANTAN TENGAH",
                                                     "KALIMANTAN SELATAN",
                                                     "KALIMANTAN TIMUR",
                                                     "KALIMANTAN BARAT"))
sumatera_ras = rasterize(sumatera_shp, IDN_ras)
kalimantan_ras = rasterize(kalimantan_shp, IDN_ras)
nw_idn_ras = trim(merge(sumatera_ras, kalimantan_ras))

# read in model outputs
# preds_sry_seasia = brick('data/model_out/SEAsia.tif')
# # grab sd raster - need to rewrite run_parasite to include in preds_sry
# preds_sd_seasia = brick('data/model_out/mean_sd_raster.tif')[[2]]
# # add sd and CIwidth of bootstraps into preds_sry
# preds_sry_seasia = addLayer(preds_sry_seasia, 
#                             preds_sd_seasia,
#                             preds_sry_seasia[[4]] - preds_sry_seasia[[3]])
# names(preds_sry_seasia) <- c('mean',
#                              'median',
#                              'lowerCI',
#                              'upperCI',
#                              'sd',
#                              'CIwidth')
# tmp = stack(preds_sry_seasia$mean,
#             preds_sry_seasia$sd)
# writeRaster(tmp,
#             "data/model_out/model_mean_sd",
#             format="GTiff")
preds_sry_seasia = stack("data/model_out/model_mean_sd.tif")
names(preds_sry_seasia) = c("mean", "sd")

preds_sry_nw = mask(crop(preds_sry_seasia, nw_idn_ras), nw_idn_ras)

# run this script to generate the accessibility raster:
# source("code/accessibility_raster.R")
access_idn = raster("data/accessibility_raster_buffered.tif")
access_idn = mask(crop(access_idn, nw_idn_ras), nw_idn_ras)

seasia_covs <- brick('data/model_covs/SEAsia_covs.grd')

malaysia_shp = malariaAtlas::getShp(country="Malaysia", admin_level = c("admin1"))
brunei_shp = malariaAtlas::getShp(country="Brunei Darussalam")
malaysia_shp = malaysia_shp[malaysia_shp$name_1 %in% c("Sarawak","Sabah"),]
borneo_remainder = rbind(malaysia_shp, brunei_shp)

###############################################################################
# For constraint function visualisation
logistic_transform = function(x, horiz=0, rate=1){
  1 / (1 + exp(-rate*(x - horiz)))
}

beta_transform = function(x, horiz=0, alpha=10, beta=10){
  maxbeta = max(dbeta(xvals, alpha, beta))
  dbeta(x-horiz, alpha, beta)/maxbeta
}

xvals = seq(0, 1, 0.001)
cusp_val = 0.5



###############################################################################
# Static selection helper - allows for selection of pixels or catchments (looks at mean val)

static_select = function(objective_surface, pointsdf, nsites=10, poly_flag=FALSE,
                         verbose=FALSE, obj_col=FALSE){
  obj_vals = extract(objective_surface, as.data.frame(pointsdf[,c("lon", "lat")]))
  keeps = seq(1:nrow(pointsdf))[!is.na(obj_vals)]
  pointsdf = pointsdf[keeps,]
  # this chunk was for removing excluded pixels from mean catchment calculations:
  #obj_vals = sapply(1:nrow(pointsdf), function(x){
  #  tmp = trim(mask(objective_surface, pointsdf[x,])) # mask objective surface to poly .. 
  #  # not sure if the trim will actually speed things up as I think it's probably 
  #  # the masking that takes most of the comp time
  #  return(mean(getValues(tmp), na.rm=TRUE)) # summarise
  #})
  if (poly_flag == TRUE){
    pointsdf = as.data.frame(pointsdf[,2:ncol(pointsdf)])
    obj_vals = pointsdf[,obj_col]
    pointsdf$keeps = keeps
    # discard polys - still identifiable through $keeps column
  }
  
  # this next bit is feeling a bit convoluted :/
  if (verbose == TRUE){
    return(pointsdf[order(obj_vals, decreasing = TRUE)[1:nsites],])
  } else {
    if (poly_flag == FALSE){
      return(order(obj_vals, decreasing=TRUE)[1:nsites])
    } else {
      return(keeps[order(obj_vals, decreasing=TRUE)[1:nsites]])
    }
  }
}

missing_points = function(old, new){
  # to compare constrained and unconstrained sets
  return(setdiff(as.data.frame(old[,c("lon","lat")]), as.data.frame(new[,c("lon","lat")])))
}

###############################################################################
# Greedy site selection
# adapt for catchments

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
# Organise some constraints

# rescale access raster from travel time (minutes) to relative accessibility
# (in line with urban_access covariate of Shearer et al. 2016 model, 
# although this is the updated surface)
values(access_idn) = (values(access_idn) - minValue(access_idn)) / 
  (maxValue(access_idn) - minValue(access_idn))
values(access_idn) = abs(values(access_idn) - 1)

# ACCESSIBIIITY CONSTRAINT MASK
prop_removed = 0.2
access_constraint_mask = access_idn
access_constraint_mask[access_idn <= quantile(access_idn, probs = prop_removed)] = NA
values(access_constraint_mask)[!is.na(values(access_constraint_mask))] = 1


# FOREST CONSTRAINT MASK
intact_forest = projectRaster(seasia_covs$forest_intact, preds_sry_nw$mean)
intact_forest = mask(intact_forest, preds_sry_nw$mean)
disturbed_forest = projectRaster(seasia_covs$forest_disturbed, preds_sry_nw$mean)
disturbed_forest = mask(disturbed_forest, preds_sry_nw$mean)

# aggregated forest surface: intact + disturbed values
forest_aggregate = nw_idn_ras
values(forest_aggregate)[!is.na(values(forest_aggregate))] = 0
values(forest_aggregate)[!is.na(values(intact_forest))] = values(intact_forest)[!is.na(values(intact_forest))]
values(forest_aggregate)[!is.na(values(disturbed_forest))] = values(forest_aggregate)[!is.na(values(disturbed_forest))] + values(disturbed_forest)[!is.na(values(disturbed_forest))]

prop_removed = 0.2
forest_constraint_mask = forest_aggregate
forest_constraint_mask[forest_aggregate <= quantile(forest_aggregate, probs = prop_removed)] = NA
values(forest_constraint_mask)[!is.na(values(forest_constraint_mask))] = 1


###############################################################################
all_pix = as.data.frame(rasterToPoints(preds_sry_nw$mean))
names(all_pix) = c("lon", "lat", "mean")

mean_sd_prod = preds_sry_nw$mean * preds_sry_nw$sd
mean_sd_quot = preds_sry_nw$mean / preds_sry_nw$sd

###############################################################################






