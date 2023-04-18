source("code/readin.R")
source("code/patternLayer.R")
source("code/plot_functions.R")
source("code/readin_catches.R")

cand_prod_catch = as.data.frame(polydf[cands$uncert_uncon_catch, c("lon", "lat")])
cand_quot_catch = as.data.frame(polydf[cands$precis_uncon_catch, c("lon", "lat")])

cand_prod_access_catch = as.data.frame(polydf[cands$uncert_access_catch, c("lon", "lat")])
cand_prod_forest_catch = as.data.frame(polydf[cands$uncert_forest_catch, c("lon", "lat")])
cand_prod_dist_catch = as.data.frame(polydf[cands$uncert_dist_catch, c("lon", "lat")])
cand_quot_access_catch = as.data.frame(polydf[cands$precis_access_catch, c("lon", "lat")])
cand_quot_forest_catch = as.data.frame(polydf[cands$precis_forest_catch, c("lon", "lat")])
cand_quot_dist_catch = as.data.frame(polydf[cands$precis_dist_catch, c("lon", "lat")])

plot_box = extent(trim(kalimantan_ras))


{png(paste0(plotpath, "constrained_all_catches.png"),
     width = 2500,
     height = 2000,
     pointsize = 30)#,
     # width = 10000,
     # height = 8000,
     # pointsize = 30,
     # res=300)
  
  par(mfrow=c(3,2), mar=c(0,0,0.1,0.1), oma=c(9.1,3.1,0.1,0.1), xpd=TRUE)
  check1 = par()
  
  # A. uncertainty-weighted access constrained
  plot(mean_sd_prod, col=excluded_col, legend=FALSE,
       xlab="", ylab="", xaxt="n", yaxt="n", legend.mar=-1)
  plot(mean_sd_prod*access_constraint_mask, col=purps(100), 
       legend=FALSE, legend.mar=6, add=TRUE)
  plot(borneo_remainder, border=darkpurp, add=TRUE)
  patternLayer(borneo_sf, "right2left", density=2, col=darkpurp, add=TRUE)
  save.usr = par()$usr
  mtext("Accessibility", 2, 1, cex=1.3)
  points(missing_points(cand_prod_catch, cand_prod_access_catch), col=excluded_point, pch=4, lwd=8)
  points(cand_prod_access_catch, col=point_col, pch=4, lwd=8)
  sel=c(1,4,6)
  text(cand_prod_access_catch$lon[sel]-0.5, cand_prod_access_catch$lat[sel]-0.1, 
       col=lab_col, labels=sel, cex=1.5)
  text(cand_prod_access_catch$lon[5]+0.5, cand_prod_access_catch$lat[5]-0.12, 
       col=lab_col, labels=5, cex=1.5, adj=c(0.5,1))
  lines(c(cand_prod_access_catch$lon[7],118.5), 
        c(cand_prod_access_catch$lat[7],3), col=line_col, lwd=3)
  text(118.5, 3, col=lab_col, labels=7, cex=1.5, adj=c(0,0))
  
  text(95.5, -5.5, "A", font=2, cex=1.5)
  # will require two insets ...
  v = par("usr")
  v = c(v[1] + (v[2] - v[1])*0.38, v[1] + (v[2] - v[1])*0.545,
        v[3] + (v[4] - v[3])*0.66, v[4])
  zoom_box = c(116.2,116.7,3.05,3.55)
  lines(c(zoom_box[1],zoom_box[1],zoom_box[2],zoom_box[2],zoom_box[1]),
        c(zoom_box[3],zoom_box[4],zoom_box[4],zoom_box[3],zoom_box[3]), 
        col="grey", lwd=3)
  lines(c(zoom_box[1],v[2]), c(zoom_box[4], v[4]), col="grey", lwd=3)
  lines(c(zoom_box[1],v[2]), c(zoom_box[3], v[3]), col="grey", lwd=3)
  v <- c(
    grconvertX(v[1:2], "user", "ndc"),
    grconvertY(v[3:4], "user", "ndc")
  )
  par(omd=v, new=TRUE, mar=c(0,0,0,0), mfrow=c(1,1))
  plot(crop(mean_sd_prod, zoom_box), col=excluded_col,
       legend=FALSE, xaxt="n", yaxt="n", ann=FALSE, colNA="white", legend.mar=-1)
  plot(mean_sd_prod*access_constraint_mask, col=purps(100), add=TRUE, 
       legend=FALSE, legend.mar=6)
  points(cand_prod_access_catch, col=point_col, pch=4, cex=0.8, lwd=5)
  sel=8:10
  text(x=cand_prod_access_catch$lon[sel]+0.05, 
       y=cand_prod_access_catch$lat[sel]+c(0.05,-0.1, 0.1), 
       labels=sel, col=lab_col, cex=1.1)
  par(mfrow=c(3,2), oma=c(9.1,3.1,0.1,0.1), mar=c(0,0,0.1,0.1),
      new=TRUE, mfg=c(1,1,3,2), usr=save.usr)
  
  # v = par("usr")
  # v = c(v[1] + (v[2] - v[1])*0.9, v[2],
  #       v[3], v[3] + (v[4] - v[3])*0.25)
  # zoom_box = c(117.8,118.2,0.6,1.1)
  # lines(c(zoom_box[1],zoom_box[1],zoom_box[2],zoom_box[2],zoom_box[1]),
  #       c(zoom_box[3],zoom_box[4],zoom_box[4],zoom_box[3],zoom_box[3]), 
  #       col="grey", lwd=3)
  # lines(c(zoom_box[2],v[2]), c(zoom_box[3], v[4]), col="grey", lwd=3)
  # lines(c(zoom_box[1],v[1]), c(zoom_box[3], v[4]), col="grey", lwd=3)
  # v <- c(
  #   grconvertX(v[1:2], "user", "ndc"),
  #   grconvertY(v[3:4], "user", "ndc")
  # )
  # par(omd=v, new=TRUE, mar=c(0,0,0,0), mfrow=c(1,1))
  # plot(crop(mean_sd_prod, zoom_box), col="grey80",
  #      legend=FALSE, xaxt="n", yaxt="n", ann=FALSE, legend.mar=-1)
  # plot(mean_sd_prod*access_constraint_mask, col=purps(100), add=TRUE, 
  #      legend=FALSE, legend.mar=6)
  # points(missing_points(cand_prod_catch, cand_prod_access_catch), col="blue", 
  #        pch=4, lwd=5, cex=0.8)
  # points(cand_prod_access_catch, col="red", pch=4, cex=0.8, lwd=5)
  # text(x=cand_prod_access_catch$lon+0.06, y=cand_prod_access_catch$lat+0.06, 
  #      labels=1:10, col="red", cex=1.1)
  # par(mfrow=c(3,2), oma=c(9.1,3.1,0.1,0.1), mar=c(0,0,0.1,0.1),
  #     new=TRUE, mfg=c(1,2,3,2), usr=save.usr)
  
  # inset for 2,3
  v = par("usr")
  v = c(v[1] + (v[2] - v[1])*0.9, v[2],
        v[3], v[3] + (v[4] - v[3])*0.2)
  zoom_box = c(118.5, 118.65,2.24,2.3)
  lines(c(zoom_box[1],zoom_box[1],zoom_box[2],zoom_box[2],zoom_box[1]),
        c(zoom_box[3],zoom_box[4],zoom_box[4],zoom_box[3],zoom_box[3]), 
        col="grey", lwd=3)
  lines(c(zoom_box[2],v[2]), c(zoom_box[4], v[4]), col="grey", lwd=3)
  lines(c(zoom_box[1],v[1]), c(zoom_box[3], v[4]), col="grey", lwd=3)
  v <- c(
    grconvertX(v[1:2], "user", "ndc"),
    grconvertY(v[3:4], "user", "ndc")
  )
  par(omd=v, new=TRUE, mar=c(0,0,0,0), mfrow=c(1,1))
  plot(crop(mean_sd_prod, zoom_box),
       legend=FALSE, xaxt="n", yaxt="n", ann=FALSE, colNA="white", legend.mar=-1)
  plot(mean_sd_prod, col=purps(100), add=TRUE, legend=FALSE, legend.mar=6)
  # fix colour scheme to larger plot
  points(cand_prod_catch, col=point_col, pch=4, cex=0.9, lwd=8)
  text(x=cand_prod_catch$lon, y=cand_prod_catch$lat-0.04, labels=1:10, col=lab_col, cex=1.1)
  # reset par after inset plot
  par(mfrow=c(3,2), oma=c(9.1,3.1,0.1,0.1), mar=c(0,0,0.1,0.1),
      new=TRUE, mfg=c(1,2,3,2), usr=save.usr)

  # B. precision-weighted access constrained
  plot(mean_sd_quot, col=excluded_col, legend=FALSE,
       xlab="", xaxt="n", ylab="", yaxt="n", legend.mar=-1)
  plot(mean_sd_quot*access_constraint_mask, col=purps(100), 
       legend=FALSE, legend.mar=6, add=TRUE)
  plot(borneo_remainder, border=darkpurp, add=TRUE)
  patternLayer(borneo_sf, "right2left", density=2, col=darkpurp, add=TRUE)
  points(cand_quot_access_catch, col=point_col, pch=4, lwd=8)
  text(95.5, -5.5, "B", font=2, cex=1.5)
  sel=9:10
  plot_sites_radius_multicrit(cand_quot_access_catch[sel,],
                              label_radius = (plot_box[2] - plot_box[1])/2*1.1, # using x axis
                              centre = c(112.5,-0.1),
                              ranked_names = sel, gap=0.92,
                              rank_cutoff = FALSE, n_toadstools=60,
                              lab_col=lab_col, line_col=line_col)
  
  v = par("usr")
  v = c(v[1] + (v[2] - v[1])*0.35, v[1] + (v[2] - v[1])*0.515,
        v[3] + (v[4] - v[3])*0.66, v[4])
  zoom_box = c(111.7,111.9,-0.6,-0.37)
  lines(c(zoom_box[1],zoom_box[1],zoom_box[2],zoom_box[2],zoom_box[1]),
        c(zoom_box[3],zoom_box[4],zoom_box[4],zoom_box[3],zoom_box[3]), 
        col="grey", lwd=3)
  lines(c(zoom_box[2],v[2]), c(zoom_box[4], v[4]), col="grey", lwd=3)
  lines(c(zoom_box[1],v[1]), c(zoom_box[3], v[3]), col="grey", lwd=3)
  v <- c(
    grconvertX(v[1:2], "user", "ndc"),
    grconvertY(v[3:4], "user", "ndc")
  )
  par(omd=v, new=TRUE, mar=c(0,0,0,0), mfrow=c(1,1))
  plot(crop(mean_sd_quot, zoom_box), col=excluded_col,
       legend=FALSE, xaxt="n", yaxt="n", ann=FALSE, colNA="white", legend.mar=-1)
  plot(mean_sd_quot*access_constraint_mask, col=purps(100), add=TRUE, 
       legend=FALSE, legend.mar=6)
  points(cand_quot_access_catch, col=point_col, pch=4, cex=0.8, lwd=5)
  text(x=cand_quot_access_catch$lon+0.02, y=cand_quot_access_catch$lat+0.02, 
       labels=1:10, col=lab_col, cex=1.1)
  par(mfrow=c(3,2), oma=c(9.1,3.1,0.1,0.1), mar=c(0,0,0.1,0.1),
      new=TRUE, mfg=c(2,1,3,2), usr=save.usr)
  
  
  # C. uncertainty-weighted forest constrained
  plot(mean_sd_prod, col=excluded_col,
       legend=FALSE, legend.mar=-1,
       xlab="", ylab="", yaxt="n", xaxt="n")
  plot(mean_sd_prod*forest_constraint_mask, col=purps(100), 
       legend=FALSE, legend.mar=6, add=TRUE)
  plot(borneo_remainder, border=darkpurp, add=TRUE)
  patternLayer(borneo_sf, "right2left", density=2, col=darkpurp, add=TRUE)
  mtext("Forest", 2, 1, cex=1.3)
  points(missing_points(cand_prod_catch, cand_prod_forest_catch), pch=4, lwd=8, col=excluded_point)
  points(cand_prod_forest_catch, pch=4, lwd=8, col=point_col) # interesting ... removes a lot of coastal sel points
  text(95.5, -5.5, "C", font=2, cex=1.5)
  sel = c(3,8,10)
  plot_sites_radius_multicrit(cand_prod_forest_catch[sel,],
                              label_radius = (plot_box[2] - plot_box[1])/1.02,
                              centre = c(116.5,4),
                              ranked_names = sel, rank_cutoff = FALSE,
                              n_toadstools=80,
                              lab_col=lab_col, line_col=line_col)
  
  v = par("usr")
  v = c(v[1] + (v[2] - v[1])*0.65, v[1] + (v[2] - v[1])*0.8,
        v[3] + (v[4] - v[3])*0.7, v[4])
  zoom_box = c(116.2,116.7,3.05,3.55)
  lines(c(zoom_box[1],zoom_box[1],zoom_box[2],zoom_box[2],zoom_box[1]),
        c(zoom_box[3],zoom_box[4],zoom_box[4],zoom_box[3],zoom_box[3]), 
        col="grey", lwd=3)
  lines(c(zoom_box[1],v[2]), c(zoom_box[4], v[4]), col="grey", lwd=3)
  lines(c(zoom_box[1],v[2]), c(zoom_box[3], v[3]), col="grey", lwd=3)
  v <- c(
    grconvertX(v[1:2], "user", "ndc"),
    grconvertY(v[3:4], "user", "ndc")
  )
  par(omd=v, new=TRUE, mar=c(0,0,0,0), mfrow=c(1,1))
  plot(crop(mean_sd_prod, zoom_box), col=excluded_col,
       legend=FALSE, xaxt="n", yaxt="n", ann=FALSE, colNA="white", legend.mar=-1)
  plot(mean_sd_prod*forest_constraint_mask, col=purps(100), add=TRUE, 
       legend=FALSE, legend.mar=6)
  points(cand_prod_forest_catch, col=point_col, pch=4, cex=0.8, lwd=5)
  sel=c(2,4,6,7)
  text(x=cand_prod_forest_catch$lon[sel]+0.05, 
       y=cand_prod_forest_catch$lat[sel]+c(0.03,0.03,0.01,0.03), 
       labels=sel, col=lab_col, cex=1.1)
  par(mfrow=c(3,2), oma=c(9.1,3.1,0.1,0.1), mar=c(0,0,0.1,0.1),
      new=TRUE, mfg=c(2,1,3,2), usr=save.usr)
  
  v = par("usr")
  v = c(v[1] + (v[2] - v[1])*0.46, v[1] + (v[2] - v[1])*0.55,
        v[3] + (v[4] - v[3])*0.54, v[3] + (v[4] - v[3])*0.69)
  zoom_box = c(115.01,115.07,1.93,1.98)
  lines(c(zoom_box[1],zoom_box[1],zoom_box[2],zoom_box[2],zoom_box[1]),
        c(zoom_box[3],zoom_box[4],zoom_box[4],zoom_box[3],zoom_box[3]), 
        col="grey", lwd=3)
  lines(c(zoom_box[1],v[2]), c(zoom_box[4], v[4]), col="grey", lwd=3)
  lines(c(zoom_box[1],v[2]), c(zoom_box[3], v[3]), col="grey", lwd=3)
  v <- c(
    grconvertX(v[1:2], "user", "ndc"),
    grconvertY(v[3:4], "user", "ndc")
  )
  par(omd=v, new=TRUE, mar=c(0,0,0,0), mfrow=c(1,1))
  plot(crop(mean_sd_prod, zoom_box), col=excluded_col,
       legend=FALSE, xaxt="n", yaxt="n", ann=FALSE, colNA="white", legend.mar=-1)
  plot(mean_sd_prod*forest_constraint_mask, col=purps(100), add=TRUE, 
       legend=FALSE, legend.mar=6)
  points(cand_prod_forest_catch, col=point_col, pch=4, cex=0.8, lwd=5)
  sel=c(1,5)
  text(x=cand_prod_forest_catch$lon[sel]+0.02, 
       y=cand_prod_forest_catch$lat[sel], 
       labels=sel, col=lab_col, cex=1.1)
  par(mfrow=c(3,2), oma=c(9.1,3.1,0.1,0.1), mar=c(0,0,0.1,0.1),
      new=TRUE, mfg=c(2,1,3,2), usr=save.usr)
  
  v = par("usr")
  v = c(v[1] + (v[2] - v[1])*0.9, v[2],
        v[3], v[3] + (v[4] - v[3])*0.25)
  zoom_box = c(117.8,118.2,0.6,1.1)
  lines(c(zoom_box[1],zoom_box[1],zoom_box[2],zoom_box[2],zoom_box[1]),
        c(zoom_box[3],zoom_box[4],zoom_box[4],zoom_box[3],zoom_box[3]), 
        col="grey", lwd=3)
  lines(c(zoom_box[2],v[2]), c(zoom_box[3], v[4]), col="grey", lwd=3)
  lines(c(zoom_box[1],v[1]), c(zoom_box[3], v[4]), col="grey", lwd=3)
  v <- c(
    grconvertX(v[1:2], "user", "ndc"),
    grconvertY(v[3:4], "user", "ndc")
  )
  par(omd=v, new=TRUE, mar=c(0,0,0,0), mfrow=c(1,1))
  plot(crop(mean_sd_prod, zoom_box), col=excluded_col,
       legend=FALSE, xaxt="n", yaxt="n", ann=FALSE, legend.mar=-1)
  plot(mean_sd_prod*forest_constraint_mask, col=purps(100), add=TRUE, 
       legend=FALSE, legend.mar=6)
  points(missing_points(cand_prod_catch, cand_prod_forest_catch), col=excluded_point, 
         pch=4, lwd=5, cex=0.8)
  points(cand_prod_forest_catch, col=point_col, pch=4, cex=0.8, lwd=5)
  text(x=cand_prod_forest_catch$lon+0.06, y=cand_prod_forest_catch$lat+0.06, 
       labels=1:10, col=lab_col, cex=1.1)
  par(mfrow=c(3,2), oma=c(9.1,3.1,0.1,0.1), mar=c(0,0,0.1,0.1),
      new=TRUE, mfg=c(2,2,3,2), usr=save.usr)
  
  
  # D. precision-weighted forest constrained
  plot(mean_sd_quot, col=excluded_col,
       legend=FALSE, legend.mar=-1, xlab="", ylab="", xaxt="n", yaxt="n")
  plot(mean_sd_quot*forest_constraint_mask, col=purps(100), 
       legend=FALSE, legend.mar=6, add=TRUE)
  plot(borneo_remainder, border=darkpurp, add=TRUE)
  patternLayer(borneo_sf, "right2left", density=2, col=darkpurp, add=TRUE)
  points(cand_quot_forest_catch, pch=4, lwd=8, col=point_col)
  #missing_points(cand_quot_catch, cand_quot_forest_catch)
  text(95.5, -5.5, "D", font=2, cex=1.5)
  sel=9:10
  plot_sites_radius_multicrit(cand_quot_forest_catch[sel,],
                              label_radius = (plot_box[2] - plot_box[1])/2*1.1, # using x axis
                              centre = c(112.5,-0.1),
                              ranked_names = sel, gap=0.92,
                              rank_cutoff = FALSE, n_toadstools=60,
                              lab_col=lab_col, line_col=line_col)
  
  # inset plot
  v = par("usr")
  v = c(v[1] + (v[2] - v[1])*0.35, v[1] + (v[2] - v[1])*0.515,
        v[3] + (v[4] - v[3])*0.66, v[4])
  zoom_box = c(111.7,111.9,-0.6,-0.37)
  lines(c(zoom_box[1],zoom_box[1],zoom_box[2],zoom_box[2],zoom_box[1]),
        c(zoom_box[3],zoom_box[4],zoom_box[4],zoom_box[3],zoom_box[3]), 
        col="grey", lwd=3)
  lines(c(zoom_box[2],v[2]), c(zoom_box[4], v[4]), col="grey", lwd=3)
  lines(c(zoom_box[1],v[1]), c(zoom_box[3], v[3]), col="grey", lwd=3)
  v <- c(
    grconvertX(v[1:2], "user", "ndc"),
    grconvertY(v[3:4], "user", "ndc")
  )
  par(omd=v, new=TRUE, mar=c(0,0,0,0), mfrow=c(1,1))
  plot(crop(mean_sd_quot, zoom_box), col=excluded_col,
       legend=FALSE, xaxt="n", yaxt="n", ann=FALSE, colNA="white", legend.mar=-1)
  plot(mean_sd_quot*forest_constraint_mask, col=purps(100), add=TRUE, 
       legend=FALSE, legend.mar=6)
  # fix colour scheme to larger plot
  points(cand_quot_forest_catch, col=point_col, pch=4, cex=0.8, lwd=5)
  text(x=cand_quot_forest_catch$lon+0.02, y=cand_quot_forest_catch$lat+0.02, labels=1:10, 
       col=lab_col, cex=1.1)
  # reset par after inset plot
  par(mfrow=c(3,2), oma=c(9.1,3.1,0.1,0.1), mar=c(0,0,0.1,0.1),
      new=TRUE, mfg=c(3,1,3,2), usr=save.usr)
  
  
  # E. uncertainty-weighted distance constrained
  plot(mean_sd_prod, col=excluded_col,
       legend=FALSE, legend.mar=-1,
       xlab="", ylab="", yaxt="n", xaxt="n")
  plot(uncert_dist_catch$excluded_ras, col=purps(100), 
       legend=FALSE, legend.mar=6, add=TRUE)
  plot(borneo_remainder, border=darkpurp, add=TRUE)
  patternLayer(borneo_sf, "right2left", density=2, col=darkpurp, add=TRUE)
  mtext("Uncertainty-weighted", 1, 1, cex=1.3)
  mtext("Distance Between Sites (50km)", 2, 1, cex=1.3)
  points(missing_points(cand_prod_catch, cand_prod_dist_catch), pch=4, lwd=8, col=excluded_point)
  points(cand_prod_dist_catch, pch=4, lwd=8, col=point_col)
  sel=c(1,3,5,10)
  text(cand_prod_dist_catch$lon[sel]-0.35, 
       cand_prod_dist_catch$lat[sel]-0.35, labels=sel, col=lab_col, cex=1.5)
  text(cand_prod_dist_catch$lon[2], 
       cand_prod_dist_catch$lat[2], labels=2, col=lab_col, cex=1.5, adj=c(0.5,-0.5))
  text(cand_prod_dist_catch$lon[2],
       cand_prod_dist_catch$lat[7]+0.5, labels=7, col=lab_col, cex=1.5)
  lines(c(cand_prod_dist_catch$lon[7],cand_prod_dist_catch$lon[2]-0.3), 
        c(cand_prod_dist_catch$lat[7],cand_prod_dist_catch$lat[7]+0.5), col=line_col, lwd=3)
  # text(cand_prod_dist_catch$lon[4], 
  #      cand_prod_dist_catch$lat[4]-0.35, labels=4, col="red", cex=1.3)
  text(95.5, -5.5, "E", font=2, cex=1.5)
  sel=c(6,8,9)
  plot_sites_radius_multicrit(cand_prod_dist_catch[sel,],
                              label_radius = (plot_box[2] - plot_box[1])/2*1.55, # using x axis
                              centre = c(117,-2), 
                              ranked_names = sel,
                              rank_cutoff = FALSE, n_toadstools=90,
                              lab_col=lab_col, line_col=line_col)
  # inset plot
  v = par("usr")
  v = c(v[1] + (v[2] - v[1])*0.9, v[2],
        v[3], v[3] + (v[4] - v[3])*0.25)
  zoom_box = c(117.8,118.2,0.6,1.1)
  lines(c(zoom_box[1],zoom_box[1],zoom_box[2],zoom_box[2],zoom_box[1]),
        c(zoom_box[3],zoom_box[4],zoom_box[4],zoom_box[3],zoom_box[3]), 
        col="grey", lwd=3)
  lines(c(zoom_box[2],v[2]), c(zoom_box[3], v[4]), col="grey", lwd=3)
  lines(c(zoom_box[1],v[1]), c(zoom_box[3], v[4]), col="grey", lwd=3)
  v <- c(
    grconvertX(v[1:2], "user", "ndc"),
    grconvertY(v[3:4], "user", "ndc")
  )
  par(omd=v, new=TRUE, mar=c(0,0,0,0), mfrow=c(1,1))
  plot(crop(mean_sd_prod, zoom_box), col=excluded_col,
       legend=FALSE, xaxt="n", yaxt="n", ann=FALSE, legend.mar=-1)
  plot(uncert_dist_catch$excluded_ras, col=purps(100), add=TRUE, 
       legend=FALSE, legend.mar=6)
  points(missing_points(cand_prod_catch, cand_prod_dist_catch), col=excluded_point, 
         pch=4, lwd=5, cex=0.8)
  points(cand_prod_dist_catch, col=point_col, pch=4, cex=0.8, lwd=5)
  text(x=cand_prod_dist_catch$lon+0.06, y=cand_prod_dist_catch$lat+0.06, 
       labels=1:10, col=lab_col, cex=1.1)
  par(mfrow=c(3,2), oma=c(9.1,3.1,0.1,0.1), mar=c(0,0,0.1,0.1),
      new=TRUE, mfg=c(3,2,3,2), usr=save.usr)
  
  # F. precision-weighted distance constrained
  plot(mean_sd_quot, col=excluded_col,
       legend=FALSE, legend.mar=-1,
       xlab="", ylab="", xaxt="n", yaxt="n")
  plot(precis_dist_catch$excluded_ras, col=purps(100), # change mask over
       legend=FALSE, legend.mar=6, add=TRUE)
  plot(borneo_remainder, border=darkpurp, add=TRUE)
  patternLayer(borneo_sf, "right2left", density=2, col=darkpurp, add=TRUE)
  mtext("Precision-weighted", 1, 1, cex=1.3)
  points(missing_points(cand_quot_catch, cand_quot_dist_catch), pch=4, lwd=8, col=excluded_point)
  points(cand_quot_dist_catch, pch=4, lwd=8, col=point_col)
  text(95.5, -5.5, "F", font=2, cex=1.5)
  sel=c(2,3,4,5,6,8,10)
  plot_sites_radius_multicrit(cand_quot_dist_catch[sel,],
                              label_radius = (plot_box[2] - plot_box[1])/2*1.15, # using x axis
                              centre = c(112.5,-1),
                              ranked_names = sel, gap=0.94,
                              rank_cutoff = FALSE, n_toadstools=60,
                              lab_col=lab_col, line_col=line_col)
  sel=c(9,7)
  plot_sites_radius_multicrit(cand_quot_dist_catch[sel,],
                              label_radius = (plot_box[2] - plot_box[1])/2*0.9, # using x axis
                              centre = c(112.5,0),
                              ranked_names = sel,
                              rank_cutoff = FALSE, n_toadstools=80,
                              lab_col=lab_col, line_col=line_col)
  
  # chuck an inset in!
  v = par("usr")
  v = c(v[1] + (v[2] - v[1])*0.35, v[1] + (v[2] - v[1])*0.515,
        v[3] + (v[4] - v[3])*0.66, v[4])
  zoom_box = c(111.7,111.9,-0.6,-0.37)
  lines(c(zoom_box[1],zoom_box[1],zoom_box[2],zoom_box[2],zoom_box[1]),
        c(zoom_box[3],zoom_box[4],zoom_box[4],zoom_box[3],zoom_box[3]), 
        col="grey", lwd=3)
  lines(c(zoom_box[2],v[2]), c(zoom_box[4], v[4]), col="grey", lwd=3)
  lines(c(zoom_box[1],v[1]), c(zoom_box[3], v[3]), col="grey", lwd=3)
  v <- c(
    grconvertX(v[1:2], "user", "ndc"),
    grconvertY(v[3:4], "user", "ndc")
  )
  par(omd=v, new=TRUE, mar=c(0,0,0,0), mfrow=c(1,1))
  plot(crop(mean_sd_quot, zoom_box), col=excluded_col,
       legend=FALSE, xaxt="n", yaxt="n", ann=FALSE, colNA="white", legend.mar=-1)
  #plot(mean_sd_quot, col=purps(100), legend=F, legend.mar=6, add=TRUE)
  points(missing_points(cand_quot_catch, cand_quot_dist_catch), cex=0.8,
         pch=4, lwd=5, col=excluded_point)
  points(cand_quot_dist_catch, col=point_col, pch=4, cex=0.8, lwd=5)
  text(x=cand_quot_dist_catch$lon+0.02, y=cand_quot_dist_catch$lat+0.02, labels=1:10, 
       col=lab_col, cex=1.1)
  
  
  # chuck some legends in ..
  par(mfrow=c(1,1), oma=c(0,0,0,0), mar=c(0,0,0,0), new=TRUE)
  plot(0, xlab="", ylab="", xaxt="n", yaxt="n", bty="n", type="n")
  par(xpd=TRUE)
  #legend(0.85,-0.95, "Excluded sites", fill=excluded_col, cex=1.2)
  legend(0.7,-0.92, c("Excluded sites", "Site selected in absence of constraint"), 
         col = c(excluded_col, excluded_point), lty=NA,
         pch = c(22, 4), pt.cex = c(3.5, 1), lwd=c(1,7),
         pt.bg = c(excluded_col, NA),
         cex=1.2)
  par(omd=c(0.4,0.9,0,1))
  plot(mean_sd_prod, col=purps(100),
       horizontal=TRUE, legend.mar=2, legend.only=TRUE,
       legend.args=list(text="Objective value", side=3, line=0.5, cex=1.3),
       axis.args=list(at=c(0.05,0.7), labels=c("Low","High"), cex.axis=1.3))
  
  dev.off()}
