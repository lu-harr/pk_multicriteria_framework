# CONSTRAINED SITE SELECTION
# this is likely to be a bin fire so get cosy

# todo:
# - suss island mean uncertainty profiles (not kalim/sumat)
# - check number of constrained pixels - forest/access

source("code/readin.R")
source("code/plot_functions.R")
source("code/patternLayer.R")
source("code/readin_catches.R")

cand_prod = all_pix[cands$uncert_uncon,]
cand_quot = all_pix[cands$precis_uncon,]

cand_prod_access = all_pix[cands$uncert_access,]
cand_prod_forest = all_pix[cands$uncert_forest,]
cand_prod_dist = all_pix[cands$uncert_dist,]
cand_quot_access = all_pix[cands$precis_access,]
cand_quot_forest = all_pix[cands$precis_forest,]
cand_quot_dist = all_pix[cands$precis_dist,]

plot_box = extent(trim(kalimantan_ras))

{png(paste0(plotpath, "constrained_all_pix.png"),
     width = 10000,
     height = 8000,
     pointsize = 30,
     res=300)
     # width = 2500,
     # height = 2000,
     # pointsize = 30)
     
  
  par(mfrow=c(3,2), mar=c(0,0,0.1,0.1), oma=c(9.1,3.1,0.1,0.1), xpd=TRUE)
  check1 = par()
  
  # A. uncertainty-weighted access constrained
  plot(mean_sd_prod, col=excluded_col, legend=FALSE,
       xlab="", ylab="", xaxt="n", yaxt="n", legend.mar=-1)
  plot(mean_sd_prod*access_constraint_mask, col=purps(100), 
       legend=FALSE, legend.mar=6, add=TRUE)
  plot(borneo_remainder, border=darkpurp, add=TRUE)
  patternLayer(borneo_sf, "right2left", density=2, col=darkpurp, add=TRUE)
  mtext("Accessibility", 2, 1, cex=1.3)
  points(missing_points(cand_prod, cand_prod_access), col=excluded_point, pch=3, lwd=8, cex=1.1)
  points(cand_prod_access, col=point_col, pch=4, lwd=8)
  sel = c(3,4,8,10)
  plot_sites_radius_multicrit(cand_prod_access[sel,], 
                              label_radius = (plot_box[2] - plot_box[1])/2.4, # using x axis
                              centre = c(117.5,3),
                              rank_cutoff = FALSE,
                              ranked_names = sel,
                              n_toadstools=40, gap=0.9,
                              line_col=line_col, lab_col=lab_col)
  sel = c(1,2,5,6,7)
  plot_sites_radius_multicrit(cand_prod_access[sel,], 
                              label_radius = (plot_box[2] - plot_box[1])/2.6, # using x axis
                              centre = c(115,0.7),
                              ranked_names = sel, rank_cutoff = FALSE, 
                              n_toadstools=40,
                              line_col=line_col, lab_col=lab_col, gap=0.93)
  lines(c(cand_prod_access[9,1], 101), c(cand_prod_access[9,2], 4), 
        col=line_col, lwd=3)
  text(c(101), c(4), labels=c(9), col=lab_col, cex=1.5, pos=4)
  text(118.5, -5.5, "A", font=2, cex=1.5)
  # add labels back in .. grab from old constrained_all
  
  # B. precision-weighted access constrained
  plot(mean_sd_quot, col=excluded_col, legend=FALSE,
       xlab="", xaxt="n", ylab="", yaxt="n", legend.mar=-1)
  plot(mean_sd_quot*access_constraint_mask, col=purps(100), 
       legend=FALSE, legend.mar=6, add=TRUE)
  plot(borneo_remainder, border=darkpurp, add=TRUE)
  patternLayer(borneo_sf, "right2left", density=2, col=darkpurp, add=TRUE)
  save.usr = par()$usr
  
  points(cand_quot_access, col=point_col, pch=4, lwd=8)
  #missing_points(cand_quot, cand_quot_access)
  # label kalimantan sites
  kalimantan_sel = c(1,3,5,10)
  kalimantan_quot = cand_quot_access[kalimantan_sel,]
  plot_sites_radius_multicrit(kalimantan_quot,
                              label_radius = (plot_box[2] - plot_box[1])/2*1.1, # using x axis
                              centre = c(112.5,-0.1),
                              ranked_names = kalimantan_sel,
                              rank_cutoff = FALSE, n_toadstools=60,
                              lab_col=lab_col, line_col=line_col,
                              gap=0.9)
  kalimantan_sel = c(4,6,7,9)
  kalimantan_quot = cand_quot_access[kalimantan_sel,]
  plot_sites_radius_multicrit(kalimantan_quot,
                              label_radius = (plot_box[2] - plot_box[1])/2*0.85, # using x axis
                              centre = c(112.5,-0.1),
                              ranked_names = kalimantan_sel,
                              rank_cutoff = FALSE, n_toadstools=60,
                              lab_col=lab_col, line_col=line_col,
                              gap=0.9)
  text(118.5, -5.5, "B", font=2, cex=1.5)
  
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
  plot(crop(mean_sd_quot, zoom_box),
       legend=FALSE, xaxt="n", yaxt="n", ann=FALSE, colNA="white", legend.mar=-1) 
  # fix colour palette to larger plot
  plot(mean_sd_quot, add=TRUE, col=purps(100), legend.mar=6, legend=FALSE)
  points(cand_quot_access[c(2,8),], col=point_col, pch=4, cex=0.9, lwd=8)
  text(cand_quot_access[c(2,8),1], cand_quot[c(2,8),2]-0.1, labels=c(2,8), col=lab_col, cex=1.1)
  # reset par after inset plot
  par(mfrow=c(3,2), oma=c(9.1,3.1,0.1,0.1), mar=c(0,0,0.1,0.1),
      new=TRUE, mfg=c(2,1,3,2), usr=save.usr)
  par(usr=save.usr)
  
  # C. uncertainty-weighted forest constrained
  plot(mean_sd_prod, col=excluded_col,
       legend=FALSE, legend.mar=-1,
       xlab="", ylab="", yaxt="n", xaxt="n")
  plot(mean_sd_prod*forest_constraint_mask, col=purps(100), 
       legend=FALSE, legend.mar=6, add=TRUE)
  plot(borneo_remainder, border=darkpurp, add=TRUE)
  patternLayer(borneo_sf, "right2left", density=2, col=darkpurp, add=TRUE)
  mtext("Forest", 2, 1, cex=1.3)
  points(missing_points(cand_prod, cand_prod_forest), pch=3, lwd=8, col=excluded_point, cex=1.1)
  points(cand_prod_forest, pch=4, lwd=8, col=point_col) # interesting ... removes a lot of coastal sel points
  text(118.5, -5.5, "C", font=2, cex=1.5)
  sel = c(6,9,10)
  plot_sites_radius_multicrit(cand_prod_forest[sel,],
                              label_radius = (plot_box[2] - plot_box[1])/1.5,
                              centre = c(117.5,-1),
                              ranked_names = sel, rank_cutoff = FALSE,
                              n_toadstools=60,
                              line_col=line_col, lab_col=lab_col, gap=0.94)
  sel = c(1,3,8)
  plot_sites_radius_multicrit(cand_prod_forest[sel,],
                              label_radius = (plot_box[2] - plot_box[1])/1.52,
                              centre = c(116,-1),
                              ranked_names = sel, rank_cutoff = FALSE,
                              n_toadstools=60,
                              line_col=line_col, lab_col=lab_col, gap=0.93)
  sel=c(2,4)
  plot_sites_radius_multicrit(cand_prod_forest[sel,],
                              label_radius = (plot_box[2] - plot_box[1])/1.5,
                              centre = c(112,2),
                              ranked_names = sel, rank_cutoff = FALSE,
                              n_toadstools=60,
                              line_col=line_col, lab_col=lab_col)
  sel=c(5,7)
  plot_sites_radius_multicrit(cand_prod_forest[sel,],
                              label_radius = (plot_box[2] - plot_box[1])/1.4,
                              centre = c(115,2),
                              ranked_names = sel, rank_cutoff = FALSE,
                              n_toadstools=60,
                              line_col=line_col, lab_col=lab_col)
  
  # D. precision-weighted forest constrained
  plot(mean_sd_quot, col=excluded_col,
       legend=FALSE, legend.mar=-1, xlab="", ylab="", xaxt="n", yaxt="n")
  plot(mean_sd_quot*forest_constraint_mask, col=purps(100), 
       legend=FALSE, legend.mar=6, add=TRUE)
  plot(borneo_remainder, border=darkpurp, add=TRUE)
  patternLayer(borneo_sf, "right2left", density=2, col=darkpurp, add=TRUE)
  points(cand_quot_forest, pch=4, lwd=8, col=point_col)
  #missing_points(cand_quot, cand_quot_forest)
  
  kalimantan_sel = c(1,3,5,10)
  kalimantan_quot = cand_quot_forest[kalimantan_sel,]
  plot_sites_radius_multicrit(kalimantan_quot,
                              label_radius = (plot_box[2] - plot_box[1])/2*1.1, # using x axis
                              centre = c(112.5,-0.1),
                              ranked_names = kalimantan_sel,
                              rank_cutoff = FALSE, n_toadstools=60,
                              lab_col=lab_col, line_col=line_col,
                              gap=0.9)
  kalimantan_sel = c(4,6,7,9)
  kalimantan_quot = cand_quot_forest[kalimantan_sel,]
  plot_sites_radius_multicrit(kalimantan_quot,
                              label_radius = (plot_box[2] - plot_box[1])/2*0.85, # using x axis
                              centre = c(112.5,-0.1),
                              ranked_names = kalimantan_sel,
                              rank_cutoff = FALSE, n_toadstools=60,
                              lab_col=lab_col, line_col=line_col,
                              gap=0.9)
  text(118.5, -5.5, "D", font=2, cex=1.5)
  # inset plot
  v = par("usr")
  v = c(v[1], v[1] + (v[2] - v[1])*0.165,
        v[3], v[3] + (v[4] - v[3])*0.34)
  zoom_box = c(101.1,101.6,-1.75,-1.25)
  lines(c(zoom_box[1],zoom_box[1],zoom_box[2],zoom_box[2],zoom_box[1]),
        c(zoom_box[3],zoom_box[4],zoom_box[4],zoom_box[3],zoom_box[3]), 
        col="grey", lwd=3)
  lines(c(zoom_box[1],v[1]), c(zoom_box[4], v[4]), col="grey", lwd=3)
  lines(c(zoom_box[2],v[2]), c(zoom_box[3], v[3]), col="grey", lwd=3)
  v <- c(
    grconvertX(v[1:2], "user", "ndc"),
    grconvertY(v[3:4], "user", "ndc")
  )
  par(omd=v, new=TRUE, mar=c(0,0,0,0), mfrow=c(1,1))
  plot(crop(mean_sd_quot, zoom_box), col=excluded_col,
       legend=FALSE, xaxt="n", yaxt="n", ann=FALSE, colNA="white", legend.mar=-1)
  plot(mean_sd_quot*access_constraint_mask, add=TRUE, col=purps(100), legend.mar=6, legend=FALSE)# fix colour scheme to larger plot
  points(cand_quot_forest[c(2,8),], col=point_col, pch=4, cex=0.9, lwd=8)
  text(cand_quot_forest[c(2,8),1], cand_quot[c(2,8),2]-0.1, labels=c(2,8), col=lab_col, cex=1.1)
  # reset par after inset plot
  par(mfrow=c(3,2), oma=c(9.1,3.1,0.1,0.1), mar=c(0,0,0.1,0.1),
      new=TRUE, mfg=c(3,1,3,2), usr=save.usr)
  
  # E. uncertainty-weighted distance constrained
  plot(mean_sd_prod, col=excluded_col,
       legend=FALSE, legend.mar=-1,
       xlab="", ylab="", yaxt="n", xaxt="n")
  plot(uncert_dist$excluded_ras, col=purps(100), 
       legend=FALSE, legend.mar=6, add=TRUE)
  plot(borneo_remainder, border=darkpurp, add=TRUE)
  patternLayer(borneo_sf, "right2left", density=2, col=darkpurp, add=TRUE)
  mtext("Uncertainty-weighted", 1, 1, cex=1.3)
  mtext("Distance Between Sites (50km)", 2, 1, cex=1.3)
  points(missing_points(cand_prod, cand_prod_dist), pch=3, lwd=8, col=excluded_point, cex=1.1)
  points(cand_prod_dist, pch=4, lwd=8, col=point_col)
  sel = c(7,8,9)
  plot_sites_radius_multicrit(cand_prod_dist[sel,], 
                              label_radius = (plot_box[2] - plot_box[1])/1, # using x axis
                              centre = c(117,-4.5),
                              ranked_names = sel, gap = 0.97,
                              rank_cutoff = FALSE, n_toadstools=80,
                              line_col=line_col, lab_col=lab_col)
  sel = c(3,4,10)
  plot_sites_radius_multicrit(cand_prod_dist[sel,],
                              label_radius = (plot_box[2] - plot_box[1])/1.57,
                              centre = c(116,-1),
                              ranked_names = sel, rank_cutoff = FALSE,
                              n_toadstools=60,
                              line_col=line_col, lab_col=lab_col, gap=0.93)
  sel = c(1,2,5,6)
  plot_sites_radius_multicrit(cand_prod_dist[sel,], 
                              label_radius = (plot_box[2] - plot_box[1])/2.6, # using x axis
                              centre = c(115,0.7),
                              ranked_names = sel,
                              rank_cutoff = FALSE, n_toadstools=35,
                              line_col=line_col, lab_col=lab_col, gap=0.94)
  text(118.5, -5.5, "E", font=2, cex=1.5)
  
  # F. precision-weighted distance constrained
  plot(mean_sd_quot, col=excluded_col,
       legend=FALSE, legend.mar=-1,
       xlab="", ylab="", xaxt="n", yaxt="n")
  plot(precis_dist$excluded_ras, col=purps(100), # change mask over
       legend=FALSE, legend.mar=6, add=TRUE)
  plot(borneo_remainder, border=darkpurp, add=TRUE)
  patternLayer(borneo_sf, "right2left", density=2, col=darkpurp, add=TRUE)
  mtext("Precision-weighted", 1, 1, cex=1.3)
  points(missing_points(cand_quot, cand_quot_dist), pch=3, lwd=8, col=excluded_point, cex=1.1)
  points(cand_quot_dist, pch=4, lwd=8, col=point_col)
  text(118.5, -5.5, "F", font=2, cex=1.5)
  # label kalimantan sites
  plot_box = extent(trim(kalimantan_ras))
  sel=c(1,3,5,8,10)
  plot_sites_radius_multicrit(cand_quot_dist[sel,], 
                              label_radius = (plot_box[2] - plot_box[1])/2*1.1, # using x axis
                              centre = c(112.5,-0.1),
                              ranked_names = sel,
                              rank_cutoff = FALSE, n_toadstools=60, gap=0.93,
                              line_col=line_col, lab_col=lab_col)
  sel=c(4,6,7)
  plot_sites_radius_multicrit(cand_quot_dist[sel,], 
                              label_radius = (plot_box[2] - plot_box[1])/2*0.85, # using x axis
                              centre = c(112.5,-0.1),
                              ranked_names = sel,
                              rank_cutoff = FALSE, n_toadstools=60, gap=0.93,
                              line_col=line_col, lab_col=lab_col)
  # label sumatera sites
  sel=c(2,9)
  plot_sites_radius_multicrit(cand_quot_dist[sel,], 
                              label_radius = (plot_box[2] - plot_box[1])/2*1.8, # using x axis
                              centre = c(107, -1),
                              ranked_names = sel,
                              rank_cutoff = FALSE, n_toadstools=40,
                              line_col=line_col, lab_col=lab_col)
  
  # chuck some legends in ..
  par(mfrow=c(1,1), oma=c(0,0,0,0), mar=c(0,0,0,0), new=TRUE)
  plot(0, xlab="", ylab="", xaxt="n", yaxt="n", bty="n", type="n")
  par(xpd=TRUE)
  #0.7,-0.9
  legend(0.7,-0.92, c("Excluded sites", "Site selected in absence of constraint"), 
         col = c(excluded_col, excluded_point), lty=NA,
         pch = c(22, 3), pt.cex = c(3.5, 1), lwd=c(1,7),
         pt.bg = c(excluded_col, NA),
         cex=1.2)
  #legend(0.7,-1, "Site selected in absence of constraint", 
  #       col=excluded_point, cex=1.2, bty="n", pch=4, lwd=8, lty=NA)
  par(omd=c(0.4,0.9,0,1))
  plot(mean_sd_prod, col=purps(100),
       horizontal=TRUE, legend.mar=2, legend.only=TRUE,
       legend.args=list(text="Objective value", side=3, line=0.5, cex=1.5),
       axis.args=list(at=c(0.05,0.7), labels=c("Low","High"), cex.axis=1.3))
  
  dev.off()}
