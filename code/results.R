# code to deal with catchments - writes outputs into /data/
source("code/readin.R")

###############################################################################
# Let's add distance-based catchments

distcatches = (apply(all_pix[,c("lon","lat")], 1,
                     getDistanceCatchmentMulticrit,
                     radius=10000, # 10km radius
                     precis=preds_sry_nw$mean/preds_sry_nw$sd,
                     uncert=preds_sry_nw$mean*preds_sry_nw$sd))
# wrangle into an SPDF ..
distcatches = do.call(rbind, distcatches)
sp = bind(distcatches[,1])
distcatches = SpatialPolygonsDataFrame(sp, data.frame(id=1:length(sp),
                                                      precis_mean=unlist(distcatches[,2]),
                                                      precis_sd=unlist(distcatches[,3]),
                                                      uncert_mean=unlist(distcatches[,4]),
                                                      uncert_sd=unlist(distcatches[,5]),
                                                      lon=all_pix[,"lon"],
                                                      lat=all_pix[,"lat"]))


writeOGR(distcatches, dsn="data", 
         layer="polydf10km", driver="ESRI Shapefile")

###############################################################################
# site selection ..

# popped the greedy calls out ..
uncert_dist=greedy_select_mindist(mean_sd_prod, all_pix, radius=50000) 
precis_dist=greedy_select_mindist(mean_sd_quot, all_pix, radius=50000)
uncert_dist_catch=greedy_select_mindist(mean_sd_prod, as.data.frame(polydf), radius=50000, obj_col="uncrt_m")
precis_dist_catch=greedy_select_mindist(mean_sd_quot, as.data.frame(polydf), radius=50000, obj_col="prcs_mn")

cands = data.frame(uncert_uncon=static_select(mean_sd_prod, all_pix),
                   precis_uncon=static_select(mean_sd_quot, all_pix),
                   uncert_uncon_catch=static_select(mean_sd_prod, polydf, obj_col="uncrt_m", poly_flag = TRUE),
                   precis_uncon_catch=static_select(mean_sd_quot, polydf, obj_col="prcs_mn", poly_flag = TRUE),
                   uncert_access=static_select(mean_sd_prod*access_constraint_mask, all_pix),
                   precis_access=static_select(mean_sd_quot*access_constraint_mask, all_pix),
                   uncert_access_catch=static_select(mean_sd_prod*access_constraint_mask, polydf, obj_col="uncrt_m", poly_flag = TRUE),
                   precis_access_catch=static_select(mean_sd_quot*access_constraint_mask, polydf, obj_col="prcs_mn", poly_flag = TRUE),
                   uncert_forest=static_select(mean_sd_prod*forest_constraint_mask, all_pix),
                   precis_forest=static_select(mean_sd_quot*forest_constraint_mask, all_pix),
                   uncert_forest_catch=static_select(mean_sd_prod*forest_constraint_mask, polydf, obj_col="uncrt_m", poly_flag = TRUE),
                   precis_forest_catch=static_select(mean_sd_quot*forest_constraint_mask, polydf, obj_col="prcs_mn", poly_flag = TRUE),
                   uncert_dist=uncert_dist$out_points,
                   precis_dist=precis_dist$out_points,
                   uncert_dist_catch=uncert_dist_catch$out_points,
                   precis_dist_catch=precis_dist_catch$out_points)

write.csv(cands, "data/cands.csv")





