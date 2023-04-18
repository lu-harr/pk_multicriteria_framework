# Supplementary figure to highlight interaction between prediction mean and sd
# as components of a uncertainty/precision-weighted objective function

source("code/readin.R")

# Wanted to add some hists to the objective space figure ....

hist_to_end_all_hists = function(x, horiz=FALSE, flipp=FALSE, fill_col="grey80",
                                 line_col="black", breaks="Sturges", thin_axis=1,
                                 binlim=FALSE, freqlim=FALSE, # didn't want to call them xlim and ylim but that's what they are ..
                                 training_wheels=FALSE,
                                 labs=FALSE, at=FALSE, xaxt=TRUE
                                 
                                 ){
  # leaving a plot label out on purpose >:)
  tmp = hist(x, plot=FALSE, breaks=breaks)
  xleft = tmp$breaks[1:(length(tmp$breaks) - 1)]
  xright = tmp$breaks[2:length(tmp$breaks)]
  ybottom = rep(0, length(tmp$breaks) - 1)
  ytop = tmp$counts
  
  ground_ax = 1
  plotbox = c(min(xleft), max(xright), min(ybottom), max(ytop))
  if (length(binlim) == 2){plotbox[1:2] = binlim}
  if (length(freqlim) == 2){plotbox[3:4] = freqlim}
  
  if (horiz == TRUE){ # x.l -> y.t, x.r -> y.b, y.b -> x.l, y.t -> x.r
    hold1 <- xleft
    hold2 <- xright
    xleft <- ybottom
    xright <- ytop
    ytop <- hold2
    ybottom <- hold1
  }
  
  message(paste(plotbox, collapse=" "))
  
  if (horiz==TRUE & flipp==FALSE){
    ground_ax = 2
    plotbox = c(plotbox[3], plotbox[4], plotbox[1], plotbox[2])
  }
  
  if (flipp == TRUE & horiz==TRUE){
    hold1 <- xleft
    xleft <- xright
    xright <- hold1
    plotbox = c(plotbox[4], plotbox[3], plotbox[1], plotbox[2])
    ground_ax = 4
  }
  
  if (flipp == TRUE & horiz== FALSE){
    hold1 <- ybottom
    ybottom <- ytop
    ytop <- hold1
    plotbox = c(plotbox[1], plotbox[2], plotbox[4], plotbox[3])
    ground_ax = 3
  }
  
  message(paste(plotbox, collapse=" "))
  
  plot(0, type="n", xlim=plotbox[1:2], ylim=plotbox[3:4], axes=training_wheels,
       xlab="", ylab="")
  rect(xleft=xleft, ybottom=ybottom, xright=xright, ytop=ytop, col=fill_col,
       border=line_col)
  
  if (xaxt==TRUE){
    if (length(at) > 1){axis(ground_ax, at=at, labels = labs)}
    else{axis(ground_ax, at=tmp$breaks[seq(1, length(tmp$breaks), thin_axis)])}
  }
  
  return(tmp)
}


###############################################################################
# objective surface

# objective mean sd plots
means = values(preds_sry_nw$mean)
sds = values(preds_sry_nw$sd)

# IDN - wide (updated BRT output)
# means = values(preds_sry_idn$mean)
# sds = values(preds_sry_idn$sd)

# remove NAs
exclude = which(is.na(means) | is.na(sds))
means = means[-exclude]
sds = sds[-exclude]

tmp1 = raster(nrows=500, ncol=500, xmn=0, xmx=1, ymn=0, ymx=1.45)
tmp2 = raster(nrows=500, ncol=500, xmn=0, xmx=1, ymn=0, ymx=1.45)
locs = xyFromCell(tmp1, 1:ncell(tmp1))
values(tmp1) = locs[,1]*locs[,2]
locs[,2] = 1/locs[,2]
values(tmp2) = locs[,1]*locs[,2]

##############################################################################

scale_prec = means/sds
scale_prod = means*sds

scale_prec = (scale_prec - min(scale_prec))/(max(scale_prec) - min(scale_prec))
scale_prod = (scale_prod - min(scale_prod))/(max(scale_prod) - min(scale_prod))

{png(paste0(plotpath,"obj_mean_sd_surface_with_obj_hists.png"),
    width = 2400,
    height = 2760,
    pointsize = 40)
par(mar=c(9.1,1,4.1,0.5), mfrow=c(1,2), oma=c(13,3.5,0,0.1), xpd=NA)
plot(log(tmp1), col=viridis(12), 
     #xlab="", ylab="", 
     main="A. Uncertainty-weighted", legend=FALSE, legend.mar=0)
points(means[seq(1, length(means))], sds[seq(1, length(means))], 
       cex=0.7, col="white", lwd=2)
mtext("Mean predicted relative risk x standard deviation", line=0.5)
mtext(2, text="Standard Deviation", line=2)
mtext(1, text="Mean Predicted Relative Risk", line=2.5)

plot(log(tmp2), col=viridis(12),yaxt="n",
     #xlab="", #ylab="Standard Deviation", 
     main="B. Precision-weighted", legend=FALSE, legend.mar=0)
points(means[seq(1, length(means))], sds[seq(1, length(means))], 
       cex=0.7, col="white", lwd=2)
mtext("Mean predicted relative risk / standard deviation", line=0.5)
mtext(1, text="Mean Predicted Relative Risk", line=2.5)

par(mfrow=c(1,1), new=TRUE, mar=c(2.1,1,4.1,0.5), oma=c(5.5, 13.5, 0, 0.1))
plot(log(tmp2), col=viridis(12), horizontal=TRUE, legend.width=0.5, legend.shrink=0.8,
     legend.args=list(text="Objective value", side=3, line=0.7),
     axis.args=list(at=c(-6,6), labels=c("Low","High")),
     legend.only=TRUE)

par(mar=c(2,0,0,0), mfrow=c(1,1), xpd=NA, new=TRUE, oma=c(0,4.5,0,0), 
    fig=c(0.2,1,0.16,0.3))
hist_to_end_all_hists(scale_prod, breaks=50, at=seq(0,1,0.2), labs = rep("",6))
text(-0.2,1300,"Uncertainty-weighted", srt=0)
par(mar=c(0,0,2,0), mfrow=c(1,1), xpd=NA, new=TRUE, oma=c(0,4.5,0,0), 
    fig=c(0.2,1,0,0.14))
hist_to_end_all_hists(scale_prec, breaks=50, flipp=TRUE, at=seq(0,1,0.2), labs = rep("",6))
text(-0.2,1700,"Precision-weighted", srt=0)
dev.off()}








