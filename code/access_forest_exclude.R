# Creates supplementary figure of accessibility and forest surfaces, 
# with constraints applied

source("code/readin.R")

tmpaccess = access_idn
tmpaccess[access_idn <= quantile(access_idn, probs = 
                             prop_removed, ncell=NULL, na.rm=TRUE)] = NA
tmpforest = forest_aggregate
tmpforest[forest_aggregate <= quantile(forest_aggregate, 
                                       probs = prop_removed)] = NA

{png(paste0(plotpath, 'access_exclude.png'),
    width = 2500,
    height = 1400,
    pointsize = 40)
plot(nw_idn_ras, col = "darkgrey", legend = F, 
     xlab = "", ylab = "", xaxt="n", yaxt="n",
     main = "Boolean constraint: 20% Least Accessible Sites Excluded",
     cex.main=1.4, cex.lab=1.3)
plot(tmpaccess, col = viridis(100), add = TRUE, 
     legend.args=list(text="Urban accessibility", font=2, side=4, line=3, cex=1.2), 
     legend.mar=6)
legend(114, -4.5, "Excluded sites", "darkgrey")
dev.off()}

{png(paste0(plotpath, 'forest_exclude.png'),
    width = 2500,
    height = 1400,
    pointsize = 40)
plot(nw_idn_ras, col = "darkgrey", legend = F, xlab = "Longitude",
     ylab = "Latitude", 
     main = "Boolean constraint: 20% Least Forested Sites Excluded",
     cex.main=1.4, cex.lab=1.3)
plot(tmpforest, col = viridis(100), add = TRUE, 
     legend.args=list(text="Forest cover", font=2, side=4, line=3, cex=1.2), 
     legend.mar=6)
legend(114, -4.5, "Excluded sites", "darkgrey")
dev.off()}


{png(paste0(plotpath, 'access_forest_exclude.png'),
    width = 2500,
    height = 2500,
    pointsize = 40)
par(mar=c(0.1,0.1,0.1,4.1), oma=c(4.1,0.1,0.1,0.1), mfrow=c(2,1))
plot(nw_idn_ras, col = "grey80", legend = F, 
     xlab = "", ylab = "", xaxt="n", yaxt="n",
     #main = "Boolean constraint: 20% Least Accessible Sites Excluded",
     cex.main=1.4, cex.lab=1.3, legend.mar=3.5)
plot(borneo_remainder, border=darkpurp, add=TRUE, lwd=2)
patternLayer(borneo_sf, "right2left", density=2, col=darkpurp, add=TRUE, lwd=2)
plot(tmpaccess, col = viridis(100), add = TRUE, 
     legend.args=list(text="Urban accessibility", font=2, side=2, line=1, cex=1.4), 
     legend.mar=3)
text(96, -5.5, "(a)", cex=1.5)

plot(nw_idn_ras, col = "grey80", legend = F,
     xlab = "", ylab = "", xaxt="n", yaxt="n",
     #main = "Boolean constraint: 20% Least Forested Sites Excluded",
     cex.main=1.4, cex.lab=1.3, legend.mar=3.5)
plot(borneo_remainder, border=darkpurp, add=TRUE, lwd=2)
patternLayer(borneo_sf, "right2left", density=2, col=darkpurp, add=TRUE, lwd=2)
plot(tmpforest, col = viridis(100), add = TRUE, 
     legend.args=list(text="Forest cover", font=2, side=2, line=1, cex=1.4), 
     legend.mar=3.5)
text(96, -5.5, "(b)", cex=1.5)
#mtext("Boolean constraints: 20% sites excluded", 3, 1, outer=TRUE, cex=1.5)

par(mfrow=c(1,1), oma=c(0,0,0,0), mar=c(0,0,0,0), new=TRUE)
plot(0, xlab="", ylab="", xaxt="n", yaxt="n", bty="n", type="n")
par(xpd=TRUE)
legend(0.9,-0.95, "Excluded sites", fill="grey80", cex=1.5)
dev.off()}


