source("code/readin.R")
source("code/readin_catches.R")

################################################################################
# demo of catchments for methods - this version uses actual surface
# (so only *approximately* 5*5)

png(paste0(plotpath, "catch_demo.png"),
    # width = 1000,
    # height = 1105,
    width=5000,
    height=5525,
    res=300,
    pointsize=35)
tmp = trim(mask(nw_idn_ras, polydf[1100,])) # example pixel
par(mar=c(1.1,1.1,4.1,1.1), bty="n")
plot(tmp, legend=FALSE, legend.mar=-1,
     xaxt="n", yaxt="n", col="#C3DEFC", main="10km catchment", cex.main=1.5) 
# This is RStudio blue :) maybe go lighter?
abline(v=seq(extent(tmp)[1], extent(tmp)[2], length.out=6), col="grey50", lwd=2)
abline(h=seq(extent(tmp)[3], extent(tmp)[4], length.out=6), col="grey50", lwd=2)
plot(polydf[1100,], add=TRUE, lty=2, lwd=6)
points(as.data.frame(polydf[1100, c("lon", "lat")]), pch=4, cex=2, lwd=4)
dev.off()
