
source("code/readin.R")
source("code/readin_catches.R")
source("code/plot_functions.R")
source("code/patternLayer.R")

###############################################################################
# UNCONSTRAINED SITE SELECTION, WITH DISTANCE-BASED CATCHMENTS
# todo:
# - fix inset colour pals
# - suss island mean uncertainty profiles (not kalim/sumat)
# - with or without blurred raster?


cand_prod = all_pix[cands$uncert_uncon,]
cand_quot = all_pix[cands$precis_uncon,]
cand_prod_catch = as.data.frame(polydf[cands$uncert_uncon_catch, c("lon", "lat")])
cand_quot_catch = as.data.frame(polydf[cands$precis_uncon_catch, c("lon", "lat")])
plot_box = extent(trim(kalimantan_ras))

{png(paste0(plotpath, "unconstrained_all.png"),
     width = 2500,
     height = 1410,
     pointsize = 30)
     # width = 10000,
     # height = 5600,
     # pointsize = 30,
     # res=300)
  
  par(mfrow=c(2,2), mar=c(0,0,0.1,0.1), oma=c(8.1,3.1,0.1,0.1), xpd=TRUE)
  check1 = par()
  
  # A. uncertainty-weighted no catchments
  plot(mean_sd_prod, col=purps(100), legend=FALSE,
       xlab="", ylab="", xaxt="n", yaxt="n", legend.mar=-1)
  plot(borneo_remainder, border=darkpurp, add=TRUE)
  patternLayer(borneo_sf, "right2left", density=2, col=darkpurp, add=TRUE)
  save.usr = par()$usr
  mtext("Individual Pixels", 2, 1, cex=1.5)
  points(cand_prod, col=point_col, pch=4, lwd=8)
  text(118.5, -5.5, "(a)", cex=1.6)
  # label kalimantan sites
  sel = c(3,4,5)
  plot_sites_radius_multicrit(cand_prod[sel,], 
                              label_radius = (plot_box[2] - plot_box[1])/2.5,
                              centre = c(117.5,3),
                              ranked_names = sel, rank_cutoff = FALSE, 
                              n_toadstools=40, gap=0.9,
                              lab_col=lab_col, line_col=line_col)
  sel = c(1,2,6,7,9)
  plot_sites_radius_multicrit(cand_prod[sel,], 
                              label_radius = (plot_box[2] - plot_box[1])/2.55,
                              centre = c(115,0.7),
                              ranked_names = sel, rank_cutoff = FALSE, 
                              n_toadstools=40,
                              lab_col=lab_col, line_col=line_col, gap=0.92)
  sel = c(8,10)
  plot_sites_radius_multicrit(cand_prod[sel,], 
                              label_radius = (plot_box[2] - plot_box[1])/1.2,
                              centre = c(116.5,2),
                              ranked_names = sel, rank_cutoff = FALSE, 
                              n_toadstools=40,
                              lab_col=lab_col, line_col=line_col)
  par(usr=save.usr)
  
  # B. precision-weighted no catchments
  plot(mean_sd_quot, col=purps(100), legend=FALSE,
       xlab="", xaxt="n", ylab="", yaxt="n", legend.mar=-1)
  plot(borneo_remainder, border=darkpurp, add=TRUE)
  patternLayer(borneo_sf, "right2left", density=2, col=darkpurp, add=TRUE)
  save.usr = par()$usr
  
  points(cand_quot, col=point_col, pch=4, lwd=8)
  # label kalimantan sites
  kalimantan_sel = c(1,3,5,10)
  kalimantan_quot = cand_quot[kalimantan_sel,]
  plot_sites_radius_multicrit(kalimantan_quot,
                              label_radius = (plot_box[2] - plot_box[1])/2*1.1, # using x axis
                              centre = c(112.5,-0.1),
                              ranked_names = kalimantan_sel,
                              rank_cutoff = FALSE, n_toadstools=60,
                              lab_col=lab_col, line_col=line_col,
                              gap=0.9)
  kalimantan_sel = c(4,6,7,9)
  kalimantan_quot = cand_quot[kalimantan_sel,]
  plot_sites_radius_multicrit(kalimantan_quot,
                              label_radius = (plot_box[2] - plot_box[1])/2*0.85, # using x axis
                              centre = c(112.5,-0.1),
                              ranked_names = kalimantan_sel,
                              rank_cutoff = FALSE, n_toadstools=60,
                              lab_col=lab_col, line_col=line_col,
                              gap=0.9)
  text(118.5, -5.5, "(b)", cex=1.6)
  
  # inset plot
  v = par("usr")
  v = c(v[1], v[1] + (v[2] - v[1])*0.165,
        v[3], v[3] + (v[4] - v[3])*0.34)
  zoom_box = c(101.1,101.6,-1.75,-1.25)
  lines(c(zoom_box[1],zoom_box[1],zoom_box[2],zoom_box[2],zoom_box[1]),
        c(zoom_box[3],zoom_box[4],zoom_box[4],zoom_box[3],zoom_box[3]), 
        col="grey80", lwd=3)
  lines(c(zoom_box[1],v[1]), c(zoom_box[4], v[4]), col="grey", lwd=3)
  lines(c(zoom_box[2],v[2]), c(zoom_box[3], v[3]), col="grey", lwd=3)
  v <- c(
    grconvertX(v[1:2], "user", "ndc"),
    grconvertY(v[3:4], "user", "ndc")
  )
  par(omd=v, new=TRUE, mar=c(0,0,0,0), mfrow=c(1,1))
  plot(crop(mean_sd_quot, zoom_box), col=purps(100),
       legend=FALSE, xaxt="n", yaxt="n", ann=FALSE, colNA="white", legend.mar=-1) # fix colour scheme to larger plot
  points(cand_quot[c(2,8),], col=point_col, pch=4, cex=0.9, lwd=8)
  text(cand_quot[c(2,8),1], cand_quot[c(2,8),2]-0.1, labels=c(2,8), col=lab_col, cex=1.1)
  # reset par after inset plot
  par(mfrow=c(2,2), oma=c(8.1,3.1,0.1,0.1), mar=c(0,0,0.1,0.1),
      new=TRUE, mfg=c(2,1,2,2), usr=save.usr)
  
  # C. uncertainty-weighted distance constrained
  plot(mean_sd_prod, col=purps(100),
       legend=FALSE, legend.mar=-1,
       xlab="", ylab="", yaxt="n", xaxt="n")
  plot(borneo_remainder, border=darkpurp, add=TRUE)
  patternLayer(borneo_sf, "right2left", density=2, col=darkpurp, add=TRUE)
  mtext("Uncertainty-weighted", 1, 1, cex=1.5)
  mtext("Mean Objective in 10km Catchment", 2, 1, cex=1.5)
  points(cand_prod_catch, pch=4, lwd=8, col=point_col)
  text(118.5, -5.5, "(c)", cex=1.6)
  sel = c(7,8,9,10)
  plot_sites_radius_multicrit(cand_prod_catch[sel,], 
                              label_radius = (plot_box[2] - plot_box[1])/1.5,
                              centre = c(118.5,-1),
                              ranked_names = sel, rank_cutoff = FALSE, 
                              n_toadstools=60,
                              lab_col=lab_col, line_col=line_col, gap=0.93)
  # sel=c(4,5)
  # plot_sites_radius_multicrit(cand_prod_catch[sel,], 
  #                             label_radius = (plot_box[2] - plot_box[1])/1.5,
  #                             centre = c(120,-1),
  #                             ranked_names = sel, rank_cutoff = FALSE, 
  #                             n_toadstools=60)
  sel=c(1,6)
  plot_sites_radius_multicrit(cand_prod_catch[sel,], 
                              label_radius = (plot_box[2] - plot_box[1])/1.2,
                              centre = c(98,5),
                              ranked_names = sel, rank_cutoff = FALSE, 
                              n_toadstools=60,
                              lab_col=lab_col, line_col=line_col)
  lines(rep(cand_prod_catch[5,1],2), c(cand_prod_catch[5,2], -1), col=line_col, lwd=3)
  text(cand_prod_catch[5,1], -1.4, 5, col=lab_col, cex=1.5)
  text(cand_prod_catch[4,1]-0.4, cand_prod_catch[4,2]-0.4, 4, col=lab_col, cex=1.5)
  
  # inset for 2,3
  v = par("usr")
  v = c(v[1] + (v[2] - v[1])*0.54, v[1] + (v[2] - v[1])*0.705,
        v[3] + (v[4] - v[3])*0.75, v[4])
  zoom_box = c(118.5, 118.65,2.24,2.3)
  lines(c(zoom_box[1],zoom_box[1],zoom_box[2],zoom_box[2],zoom_box[1]),
        c(zoom_box[3],zoom_box[4],zoom_box[4],zoom_box[3],zoom_box[3]), 
        col="grey80", lwd=3)
  lines(c(zoom_box[2],v[2]), c(zoom_box[4], v[4]), col="grey", lwd=3)
  lines(c(zoom_box[1],v[2]), c(zoom_box[3], v[3]), col="grey", lwd=3)
  v <- c(
    grconvertX(v[1:2], "user", "ndc"),
    grconvertY(v[3:4], "user", "ndc")
  )
  par(omd=v, new=TRUE, mar=c(0,0,0,0), mfrow=c(1,1))
  plot(crop(mean_sd_prod, zoom_box),
       legend=FALSE, xaxt="n", yaxt="n", ann=FALSE, colNA="white", legend.mar=-1, 
       bg="white")
  plot(mean_sd_prod, col=purps(100), add=TRUE, legend=FALSE, legend.mar=6)
  # fix colour scheme to larger plot
  points(cand_prod_catch, col=point_col, pch=4, cex=0.9, lwd=8)
  text(x=cand_prod_catch$lon, y=cand_prod_catch$lat-0.04, labels=1:10, col=lab_col, cex=1.1)
  # reset par after inset plot
  par(mfrow=c(2,2), oma=c(8.1,3.1,0.1,0.1), mar=c(0,0,0.1,0.1),
      new=TRUE, mfg=c(2,2,2,2), usr=save.usr)
  
  # D. precision-weighted w catchments
  plot(mean_sd_quot, col=purps(100),
       legend=FALSE, legend.mar=-1, xlab="", ylab="",
       xaxt="n", yaxt="n")
  mtext("Precision-weighted", 1, 1, cex=1.5)
  plot(borneo_remainder, border=darkpurp, add=TRUE)
  patternLayer(borneo_sf, "right2left", density=2, col=darkpurp, add=TRUE)
  
  points(cand_quot_catch, pch=4, lwd=8, col=point_col)
  sel = c(9,10)
  plot_sites_radius_multicrit(cand_quot_catch[sel,], 
                              label_radius = (plot_box[2] - plot_box[1])/1.05,
                              centre = c(116.5,3),
                              ranked_names = sel, rank_cutoff = FALSE, 
                              n_toadstools=40,
                              lab_col=lab_col, line_col=line_col)
  text(118.5, -5.5, "(d)", cex=1.6)
  
  # inset plot
  v = par("usr")
  v = c(v[1] + (v[2] - v[1])*0.35, v[1] + (v[2] - v[1])*0.515,
        v[3] + (v[4] - v[3])*0.66, v[4])
  zoom_box = c(111.7,111.9,-0.6,-0.37)
  lines(c(zoom_box[1],zoom_box[1],zoom_box[2],zoom_box[2],zoom_box[1]),
        c(zoom_box[3],zoom_box[4],zoom_box[4],zoom_box[3],zoom_box[3]), 
        col="grey80", lwd=3)
  lines(c(zoom_box[2],v[2]), c(zoom_box[4], v[4]), col="grey", lwd=3)
  lines(c(zoom_box[1],v[1]), c(zoom_box[3], v[3]), col="grey", lwd=3)
  v <- c(
    grconvertX(v[1:2], "user", "ndc"),
    grconvertY(v[3:4], "user", "ndc")
  )
  par(omd=v, new=TRUE, mar=c(0,0,0,0), mfrow=c(1,1))
  # initial plot gives us the right area:
  plot(crop(mean_sd_quot, zoom_box),
       legend=FALSE, xaxt="n", yaxt="n", ann=FALSE, colNA="white", legend.mar=-1)
  # then replot to get the colour palette in line with the rest of the plot:
  plot(mean_sd_quot, col=purps(100), add=TRUE, legend.mar=6, legend=FALSE)
  points(cand_quot_catch, col=point_col, pch=4, cex=0.9, lwd=8)
  text(x=cand_quot_catch$lon+0.02, y=cand_quot_catch$lat+0.02, labels=1:10, 
       col=lab_col, cex=1.1)
  # reset par after inset plot
  par(mfrow=c(2,2), oma=c(8.1,3.1,0.1,0.1), mar=c(0,0,0.1,0.1),
      new=TRUE, mfg=c(2,2,2,2), usr=save.usr)
  check4 = par()
  
  par(mfrow=c(1,1), oma=c(0,0,0,0), mar=c(0,0,0,0), new=TRUE)
  plot(0, xlab="", ylab="", xaxt="n", yaxt="n", bty="n", type="n")
  par(xpd=TRUE)
  #legend(0.85,-0.9, "Excluded sites", fill="grey80")
  par(omd=c(0.25,0.75,0,1))
  plot(mean_sd_prod, col=purps(100),
       horizontal=TRUE, legend.mar=2, legend.only=TRUE,
       legend.args=list(text="Objective value", side=3, line=0.5, cex=1.5),
       axis.args=list(at=c(0.05,0.7), labels=c("Low","High"), cex.axis=1.5))
  
  dev.off()}
