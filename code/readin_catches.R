# script to skip steps in results.R by reading in intermediate outputs
# source("code/readin.R")
# source("code/results.R")


# Read in pre-prepared outputs of 1_results.R
polydf = readOGR("data/results/polydf10km.shp")
cands = read.csv("data/results/cands.csv")

# Excluded rasters are attached to greedy() output for each case
# (In plotting, they're equivalent to constraint_masks for access and forest above)
uncert_dist = greedy_select_mindist(mean_sd_prod, all_pix, radius=50000)
precis_dist = greedy_select_mindist(mean_sd_quot, all_pix, radius=50000)
uncert_dist_catch = greedy_select_mindist(mean_sd_prod, as.data.frame(polydf), radius=50000, obj_col="uncrt_m")
precis_dist_catch = greedy_select_mindist(mean_sd_quot, as.data.frame(polydf), radius=50000, obj_col="prcs_mn")

# "Smoothed" rasters: pixel values are mean objective value for pixel's 10km 
# radius catchment
smooth_prod = nw_idn_ras
smooth_quot = nw_idn_ras
values(smooth_prod)[!is.na(values(smooth_prod))] = polydf$uncrt_m
values(smooth_quot)[!is.na(values(smooth_quot))] = polydf$prcs_mn
# not used in body of paper except for autocorrelation statistics
# which ended up being removed