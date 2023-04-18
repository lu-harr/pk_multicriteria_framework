source("code/readin.R")

{png(paste0(plotpath, 'brt_mean_sd.png'),
     # width = 2500,
     # height = 2200,
     pointsize = 40,
     res=300,
     width=10000,
     height=8800)
  
  par(mar=c(0.1,2.1,0.1,1), mfrow=c(2,1), xpd=TRUE)
  plot(preds_sry_nw$mean, col=viridis(100),
       #main="Boosted regression tree output:\nMean predicted relative risk",
       xlab="", ylab="", xaxt="n", yaxt="n")
  mtext("Mean", 2, 1, font=2, cex=1.3)
  text(95.5, -5.5, "A", font=2, cex=1.2)
  plot(preds_sry_nw$sd, col=viridis(100),
       #main="Boosted regression tree output:\nStandard deviation of predicted relative risk",
       xlab="", ylab="", xaxt="n", yaxt="n")
  text(95.5, -5.5, "B", font=2, cex=1.2)
  mtext("Standard Deviation", 2, 1, font=2, cex=1.3)
  #par(mar=c(5,5,0,5), oma=c(0,0,1,0), mfrow=c(1,1), new=TRUE)
  #plot(0, xlab="Longitude", ylab="Latitude", bty="n", xaxt="n", yaxt="n", 
  #     type="n", cex.lab=1.3)
  dev.off()}

# Moran's I
# Moran(preds_sry_nw$mean)
# Moran(preds_sry_nw$sd)
# Moran(preds_sry_nw$mean*preds_sry_nw$sd)
# Moran(preds_sry_nw$mean/preds_sry_nw$sd)

# alternatively with hypot test
library(spdep)
run_my_moran = function(ras){
  polygs = rasterToPolygons(ras)
  nb.polygs = poly2nb(polygs, queen=TRUE)
  lw.polygs = nb2listw(nb.polygs, style="W", zero.policy=TRUE) # row-standardised
  vals = ras[!is.na(ras)]
  return(moran.test(vals, lw.polygs, zero.policy = TRUE))
}

# run_my_moran(preds_sry_nw$mean)
# run_my_moran(preds_sry_nw$sd)
# run_my_moran(preds_sry_nw$mean/preds_sry_nw$sd)
# run_my_moran(preds_sry_nw$mean*preds_sry_nw$sd)