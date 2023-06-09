# plotting functions & general setup
library(RColorBrewer)

# this looks a bit silly but trust the process
# .. for some reason the middle of the purple palette looked super blown out 
# when I threw it through colorRampPal ..
purps = brewer.pal(9, "Purples")
darkpurp = rgb(colorRamp(c(purps[9], "black"))(0.9)/100)
purps = colorRampPalette(c(purps[2], purps[9]))(9)
purps = colorRampPalette(c(purps, darkpurp))

# for point labelling:
point_col = "#fc7b03"
line_col = "#fc7b03"
lab_col = "#fc7b03"
# ^ atm running with a burnt orange over the purps

# there's a light orange here or a light green ..
#excluded_col = "#f7dfdf"
excluded_col = "#ebf5e1"
excluded_col = "#e6f1da"
# and here's an apple green ... :/
excluded_point = "#6dbd1c"

borneo_sf = st_as_sf(borneo_remainder)

my_simple_dist = function(pos1, pos2){
  # this was quicker than googling it :)))
  # for plotting only - distHaversine should be used where geographical 
  # distance is important
  return((pos1[1] - pos2[1])**2 + (pos1[2] - pos2[2])**2)
}


plot_sites_radius_multicrit = function(ranked_sites, centre, label_radius,
                                       ranked_names, rank_cutoff=FALSE, 
                                       n_toadstools=65, gap=0.95,
                                       testing=FALSE,
                                       line_col="red", lab_col="red"){
  # possibly the most geniusest piece of code the author has ever written
  # and it's just for making my plot lables nice :')
  # would be nice to rig this for longer lables though mate - set incline to angle
  # and include clause to flip if angle in pi/2-3pi/2
  
  if (rank_cutoff){  # impose (optional) supplied cutoff
    ranked_combs = ranked_combs[1:rank_cutoff,]
  }
  
  angles = (1:n_toadstools)*2*pi/n_toadstools # you know, like a fairy circle :)
  
  # possible set of label positions
  outer_circle_coords = data.frame(x=(label_radius)*cos(angles)+centre[1], 
                                   y=(label_radius)*sin(angles)+centre[2])
  # possible set of end points for the lines we're putting in
  circle_coords = data.frame(x=(label_radius*gap)*cos(angles)+centre[1], 
                             y=(label_radius*gap)*sin(angles)+centre[2])
  if (testing == TRUE){points(circle_coords)}
  
  taken_coords = c() # keep track of occupied positions in the circle
  for (i in 1:nrow(ranked_sites)){
    # find distance from point to all possible label locations
    dists_to_circle = my_simple_dist(circle_coords, c(ranked_sites[i,1],
                                                      ranked_sites[i,2]))
    # find closest label
    up = which(dists_to_circle == min(dists_to_circle))
    down = up
    
    # iterate around the circle if the label is already assigned
    while(up %in% taken_coords & down %in% taken_coords){
      down = ifelse(down <= 1, n_toadstools, down - 1)
      up = ifelse(up >= n_toadstools, 1, up + 1)
    }
    
    # pick the best outta the `up` and `down` options we're left with
    if(!(up %in% taken_coords) & !(down %in% taken_coords)){
      tmp = ifelse(dists_to_circle[up,1] > dists_to_circle[down,1], down, up)
    } else {
      tmp = ifelse(up %in% taken_coords, down, up)
    }
    outpoint = circle_coords[tmp,]
    outpoint_label = outer_circle_coords[tmp,]
    taken_coords = c(taken_coords, tmp)
    
    # we're ready to add to the plot!
    text(outpoint_label, labels=ranked_names[i], col=lab_col, cex=1.5) # needs a shift :)
    lines(c(ranked_sites[i,1], outpoint[1]), 
          c(ranked_sites[i,2], outpoint[2]), 
          col=line_col, lwd=3)
  }
}
