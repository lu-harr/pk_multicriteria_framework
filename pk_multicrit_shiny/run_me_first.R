library(shiny)
library(viridisLite)
library(dplyr)
library(DT)
library(markdown)
library(raster)
library(plotfunctions)
library(sf)
library(RColorBrewer)
library(rgeos)
library(prodlim) # this was commented out ...
library(shinyjs)
library(shinyBS)
library(malariaAtlas)
library(dismo)
library(prompter)


#setwd("~/Desktop/knowlesi/multicriteria_paper/pk_multicrit_shiny")
source("functions.R")
###############################################################################
# Study area: Kalimantan and Sumatra only ...
nw_idn_ras = raster("nw_idn_ras.grd")
ncell_nw_idn_ras = ncell(nw_idn_ras)
ncell_non_na_nw_idn_ras = ncell_nw_idn_ras - freq(nw_idn_ras, value=NA)
preds_sry_nw = stack("preds_sry_nw.grd")
#access_idn = raster("access_nwidn.grd") # no islands here - grab these things from paper dir
#forest_idn = raster("forest_nwidn.grd")

####
# from readin.R
access_idn = raster("accessibility_raster_buffered.tif")
access_idn = mask(crop(access_idn, nw_idn_ras), nw_idn_ras)
# rescale access raster from travel time (minutes) to relative accessibility
# (in line with urban_access covariate of Shearer et al. 2016 model, 
# although this is the updated surface)
values(access_idn) = (values(access_idn) - minValue(access_idn)) / 
  (maxValue(access_idn) - minValue(access_idn))
values(access_idn) = abs(values(access_idn) - 1)

# separate forest layers - only forest layers need to be included in published dir
# seasia_covs = brick("SEAsia_covs.grd")
# forest_layers = seasia_covs[[c("forest_intact", "forest_disturbed")]]
# writeRaster(forest_layers, "forest_layers.grd")
forest_layers = stack("forest_layers.grd")
intact_forest = projectRaster(forest_layers$forest_intact, preds_sry_nw$mean)
intact_forest = mask(intact_forest, preds_sry_nw$mean)
disturbed_forest = projectRaster(forest_layers$forest_disturbed, preds_sry_nw$mean)
disturbed_forest = mask(disturbed_forest, preds_sry_nw$mean)

forest_idn = nw_idn_ras
values(forest_idn)[!is.na(values(forest_idn))] = 0
values(forest_idn)[!is.na(values(intact_forest))] = values(intact_forest)[!is.na(values(intact_forest))]
values(forest_idn)[!is.na(values(disturbed_forest))] = values(forest_idn)[!is.na(values(disturbed_forest))] + values(disturbed_forest)[!is.na(values(disturbed_forest))]

####

init_thresholds = list(access_min=0,
                       access_max=100,
                       forest_min=0,
                       forest_max=100,
                       nsites=0)

# this looks a bit silly but trust the process
# .. for some reason the middle of the purple palette looked super blown out 
# when I threw it through colorRampPal ..
purps = brewer.pal(9, "Purples")
darkpurp = rgb(colorRamp(c(purps[9], "black"))(0.9)/100)
lighter_purps = colorRampPalette(c(purps[2], purps[9]))
purps = colorRampPalette(c(purps[2], purps[9]))(9)
purps = colorRampPalette(c(purps, darkpurp))

# for point labelling:
point_col = "#fc7b03"
line_col = "#fc7b03"
lab_col = "#fc7b03"
# excluded_col = "#e6f1da"
# excluded_point = "#6dbd1c"
# swapping greens (above) for blues (below) so that there's a bit more 
# differentiation for R-G colourblind users
excluded_col = "#d2f2f2"
excluded_point = "#1cbdbd"

# grab some shps ... actually need to put borneo_remainder into plots
malaysia_shp = malariaAtlas::getShp(country="Malaysia", admin_level = c("admin1"))
brunei_shp = malariaAtlas::getShp(country="Brunei Darussalam")
malaysia_shp = malaysia_shp[malaysia_shp$name_1 %in% c("Sarawak","Sabah"),]
borneo_remainder = rbind(malaysia_shp, brunei_shp)
borneo_sf = st_as_sf(borneo_remainder)
