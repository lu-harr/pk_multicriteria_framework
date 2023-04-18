# script for boolean_exclude figure
library(viridisLite)
plotpath = "/output/"

pal = viridis(5)

logistic_transform = function(x, horiz=0, rate=1){
  1 / (1 + exp(-rate*(x - horiz)))
}

beta_transform = function(x, horiz=0, alpha=10, beta=10){
  maxbeta = max(dbeta(xvals, alpha, beta))
  dbeta(x-horiz, alpha, beta)/maxbeta
}

xvals = seq(0, 1, 0.001)
cusp_val = 0.5

{png(paste0(plotpath, 'boolean_exclude.png'),
     # width = 2500,
     # height = 1400,
     # pointsize = 50)
     width = 10000,
     height = 6000,
     pointsize = 50,
     res=300)
  par(mar=c(0,0,0,0.5), oma=c(5.1,4.1,3.1,0.5), mfrow=c(1,2))
  plot(xvals, logistic_transform(xvals, cusp_val, 
                                 rate = 1/(0.025)), 
       type = "l", lwd = 10, col = pal[3],
       cex.main = 1.7, cex.lab = 1.3)
  lines(c(0, cusp_val, cusp_val, 1),
        c(0, 0, 1, 1), col = pal[1], lwd = 10)
  lines(xvals, logistic_transform(xvals, cusp_val, 
                                  rate = 1/0.05), 
        type = "l", lwd = 10, col = pal[4])
  mtext("Boolean", font=2, cex=1.5, line=1)
  plot(xvals, beta_transform(xvals), 
       type = "l", lwd = 10, col = pal[3],
       yaxt="n",
       cex.main = 1.7, cex.lab = 1.3)
  lines(c(0, cusp_val-0.1, cusp_val-0.1, cusp_val+0.1, cusp_val+0.1, 1),
        c(0, 0, 1, 1, 0, 0), col = pal[1], lwd = 10)
  lines(xvals, beta_transform(xvals, alpha=5, beta=5), 
        type = "l", lwd = 10, col = pal[4])
  mtext("Target", font=2, cex=1.5, line=1)
  mtext("Constraint Value", 1, outer=TRUE, cex=1.3, line=3)
  mtext("Relative Objective Value", 2, outer=TRUE, cex=1.3, line=3)
  
  dev.off()}