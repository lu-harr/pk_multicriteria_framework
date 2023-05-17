source("run_me_first.R")

# fixing up accepted values for numeric inputs ...
# still something funny happening for forest

server <- (function(input, output, session){
  
  # I don't think shinyBS is doing the thing for the version of shiny on shinyapps.io?
  # shinyBS::addTooltip(session=session, id="include_excluded_sites", 
  #                     title="Show sites with objective values higher than those selected, or sites that would be selected in the absence of specified constraints", 
  #                    placement = "bottom", trigger = "hover",
  #                    options = NULL)
  # 
  # shinyBS::addTooltip(session=session, id="info", 
  #                     title="Brush an area in the map to see only the captured subset of sites in the downloadable table!",
  #                    placement = "bottom", trigger = "hover",
  #                    options = NULL)
  
  # Hide sidebar when user is viewing landing tab ..
  # This is a bit janky and there's probably (definitely) a better way to do this
  observeEvent(input[["tabset"]], {
    if(input[["tabset"]] == "Get Started"){
      hideElement(selector = "#sidebar")
      removeCssClass("main", "col-sm-8")
      addCssClass("main", "col-sm-12")
    } else {
      showElement(selector="#sidebar")
      removeCssClass("main", "col-sm-12")
      addCssClass("main", "col-sm-8")
    }
  })
  
  # OBJECTIVE
  object_surface = eventReactive(input$update, {
    if (input$objective_variable == "Mean / Std Dev"){
      tmp = preds_sry_nw[["mean"]] / preds_sry_nw[["sd"]]
    } else {
      if (input$objective_variable == "Mean x Std Dev"){
        tmp = preds_sry_nw[["mean"]] * preds_sry_nw[["sd"]]
      } else {
        tmp = preds_sry_nw[["mean"]]
      }
    }
    tmp
  })
  
  constraint_tab_flag = reactiveValues(flag = FALSE)
  observeEvent(input$update, {
    constraint_tab_flag$flag = TRUE
  })
  
  
  # CONSTRAINTS
  
  # accessibility: one slider and two numeric inputs for upper and lower bound
  output$access_thresh_s = renderUI({
    sliderInput("access_thresh_slide",
                label=div(h5("Accessibility:")),
                min=0,
                max=100,
                value=c(signif(init_thresholds$access_min, 2),
                        signif(init_thresholds$access_max, 2)))
  })
  output$access_thresh_n_lower = renderUI({
    numericInput("access_thresh_nume_lower",
                 label="",
                 min=0,
                 max=100,
                 value=signif(init_thresholds$access_min, 2))
  })
  
  output$access_thresh_n_upper = renderUI({
    numericInput("access_thresh_nume_upper",
                 label="",
                 min=0,
                 max=100,
                 value=signif(init_thresholds$access_max, 2))
  })
  
  # stitch all the access inputs together
  observeEvent(input$access_thresh_slide, {
    updateNumericInput(session, "access_thresh_nume_lower", 
                       value=input$access_thresh_slide[1])
    updateNumericInput(session, "access_thresh_nume_upper", 
                       value=input$access_thresh_slide[2])
  })
  observeEvent(input$access_thresh_nume_lower, {
    updateSliderInput(session, "access_thresh_slide", 
                      value=c(input$access_thresh_nume_lower,
                              input$access_thresh_nume_upper))
  })
  observeEvent(input$access_thresh_nume_upper, {
    updateSliderInput(session, "access_thresh_slide", 
                      value=c(input$access_thresh_nume_lower,
                              input$access_thresh_nume_upper))
  })
  
  
  
  # forest: also one slider and two numeric inputs
  output$forest_thresh_s = renderUI({
    sliderInput("forest_thresh_slide",
                label=div(h5("Forest cover:")),
                min=0,
                max=100,
                value=c(signif(init_thresholds$forest_min, 2),
                        signif(init_thresholds$forest_max, 2)))
  })
  output$forest_thresh_n_lower = renderUI({
    numericInput("forest_thresh_nume_lower",
                 label="",
                 min=0,
                 max=100,
                 value=signif(init_thresholds$forest_min, 2))
  })
  output$forest_thresh_n_upper = renderUI({
    numericInput("forest_thresh_nume_upper",
                 label="",
                 min=0,
                 max=100,
                 value=signif(init_thresholds$forest_max, 2))
  })
  
  # link all forest inputs up:
  observeEvent(input$forest_thresh_slide, {
    updateNumericInput(session, "forest_thresh_nume_lower", value=input$forest_thresh_slide[1])
    updateNumericInput(session, "forest_thresh_nume_upper", value=input$forest_thresh_slide[2])
  })
  observeEvent(input$forest_thresh_nume_lower, {
    updateSliderInput(session, "forest_thresh_slide", value=c(input$forest_thresh_nume_lower,
                                                              input$forest_thresh_nume_upper))
  })
  observeEvent(input$forest_thresh_nume_upper, {
    updateSliderInput(session, "forest_thresh_slide", value=c(input$forest_thresh_nume_lower,
                                                              input$forest_thresh_nume_upper))
  })
  
  
  observeEvent(input$update, {
    # prevent nasty values :)
    updateNumericInput(session, "access_thresh_nume_lower", 
                       value=max(0, input$access_thresh_nume_lower))
    updateNumericInput(session, "access_thresh_nume_upper", 
                       value=max(input$access_thresh_nume_lower + 1,
                                 min(100, input$access_thresh_nume_upper)))
    updateNumericInput(session, "forest_thresh_nume_lower",
                       value=max(0, input$forest_thresh_nume_lower))
    updateNumericInput(session, "forest_thresh_nume_upper",
                       value=max(input$forest_thresh_nume_lower + 1,
                                 min(100, input$forest_thresh_nume_upper)))
    updateNumericInput(session, "dist_sites",
                       value=max(input$dist_sites, 0))
  })
  
  # make changes to access mask when user edits access bounds
  accessedit = eventReactive(input$update, {
    tmp = access_idn
    values(tmp)[values(access_idn) < quantile(tmp, input$access_thresh_slide[1]/100)] = NA
    values(tmp)[values(access_idn) > quantile(tmp, input$access_thresh_slide[2]/100)] = NA
    tmp
  })
  
  # make changes to forest mask given user preferences
  forestedit = eventReactive(input$update, {
    tmp = forest_idn
    values(tmp)[values(tmp) < quantile(forest_idn, input$forest_thresh_slide[1]/100)] = NA
    values(tmp)[values(tmp) > quantile(forest_idn, input$forest_thresh_slide[2]/100)] = NA
    tmp
  })
  
  
  # Putting it all together: create the overlay raster (without distance)
  # need to deal with distance constraint a little differently and apply a greedy :)
  overlay_ras = reactiveValues(r=nw_idn_ras,
                               catch_polys=list())
  
  # initialise empty ranked_sites
  ranked_sites = reactiveValues(d=setNames(data.frame(matrix(NA, ncol=4, nrow=0)), 
                                               c("lon", "lat", "obj", "rank")),
                                m=setNames(data.frame(matrix(NA, ncol=3, nrow=0)), 
                                           c("lon", "lat", "obj")))
  
  # If the "Go!" button is clicked, edit the overlay raster and rank the sites
  observeEvent(input$update, { 
    # create overlay
    maskover = accessedit()*forestedit()
    overlay_ras$r = object_surface()
    values(overlay_ras$r)[is.na(values(maskover))] = NA
    
    if (input$nsites > 0){
      # creates a df of unconstrained sites, with a column for objective value
      pointsdf = as.data.frame(rasterToPoints(isolate(overlay_ras$r)))
      names(pointsdf) = c("lon", "lat", "obj")
      
      if (input$dist_sites == 0){ # if distance constraint not applied ..
        # order + cut off
        pointsdf = pointsdf[order(pointsdf[,"obj"],
                                  decreasing = ifelse(input$objective_direction == "Maximise", TRUE, FALSE)),]
        pointsdf = as.data.frame(pointsdf[1:min(input$nsites, nrow(pointsdf)),])
        overlay_ras$catch_polys = list()
      } else {
        # require greedy selection - applied after forest + access constraints
        out = greedy_select_mindist(objective_surf=isolate(overlay_ras$r),
                                    pointsdf=pointsdf,
                                    radius=input$dist_sites*1000, # convert to m
                                    npoints=input$nsites,
                                    maximising=ifelse(input$objective_direction == "Maximise", TRUE, FALSE))
        overlay_ras$r = out$excluded_ras
        pointsdf = pointsdf[out$out_points,]
        overlay_ras$catch_polys = out$shadows
      }
      # save edited table
      ranked_sites$d = pointsdf 
      # add a rank column
      ranked_sites$d$rank = 1:nrow(ranked_sites$d)
      
      # allow user to plot sites with higher objective values than sites selected
      # ( i.e. "excluded sites")
      if (input$include_excluded_sites == TRUE){
        all_points = as.data.frame(rasterToPoints(object_surface()))
        all_points = all_points[order(all_points[,3], # obj column
                                      decreasing = ifelse(input$objective_direction == "Maximise", 
                                                          TRUE, FALSE)),]
        missing_points = all_points[1:row.match(ranked_sites$d[input$nsites,c("lon","lat")], 
                                                all_points[,1:2]),]
        ranked_sites$m = setNames(missing_points[which(! 1:nrow(missing_points) %in% row.match(pointsdf[,1:2], 
                                                                                               missing_points[,1:2])),],
                                  c("lon", "lat", "obj"))
      }
      
    } else {
      ranked_sites$d = setNames(data.frame(matrix(NA, ncol=4, nrow=0)), 
                                c("lon", "lat", "obj", "rank"))
      ranked_sites$m = setNames(data.frame(matrix(NA, ncol=3, nrow=0)), 
                                c("lon", "lat", "obj"))
    }
  })
  
  
  # overlay plot
  output$overlay_plot <- renderPlot({
    validate(need(overlay_ras$r, "Select objective and constraints!"))
    
    par(bty="n", mar=c(4.1,4.1,4.1,0.1))
    plot(nw_idn_ras, xlab="", ylab="", cex.main=2,
         legend=FALSE, col=excluded_col, xaxt="n", yaxt="n")  # green map underneath :)
    plot(overlay_ras$r, add=TRUE, 
         col = purps(100))
    legend("bottomright", "Excluded sites", fill=excluded_col, bty="n")
    
    if (nrow(ranked_sites$d) > 0){
      
      if (input$include_excluded_sites == TRUE){
        # plot excluded sites
        points(ranked_sites$m[,"lon"], ranked_sites$m[,"lat"], col=excluded_point, lwd=6, pch=4)
      }
      
      points(ranked_sites$d[,"lon"], ranked_sites$d[,"lat"], col=point_col, lwd=6, pch=4)
    }
  })
  
  # table of selected sites
  output$sites_table <- DT::renderDataTable({
    validate(need(nrow(ranked_sites$d > 0), "No sites selected!"))
    
    # allow user to "brush" subset of selected sites in the map
    if (!is.null(input$overlay_plot_brush)){
      brushedPoints(ranked_sites$d[,c("rank","lon","lat","obj")],
                    brush=input$overlay_plot_brush,
                    xvar="lon", yvar="lat") %>%
        datatable(colnames=c("Rank","Longitude", "Latitude", "Objective value"),
                  rownames=FALSE) %>%
          formatRound(columns=2:4, digits=3)
      
    } else {
      datatable(ranked_sites$d[,c("rank","lon","lat","obj")],
                colnames=c("Rank", "Longitude", "Latitude", "Objective value"),
                rownames=FALSE) %>%
        formatRound(columns=2:4, digits=3)
    }
  })  
  
  # button to download what's in the table - doesn't include the brushing step
  output$download_sites <- downloadHandler(
    filename = function(){"ranked_sites.csv"},
    content = function(fname){
      write.csv(ranked_sites$d, fname)
    }
  )
  
  # plot for the constraint tab
  output$constraint_plot <- renderPlot({
    message(exists("maskover"))
    validate(need(constraint_tab_flag$flag == TRUE, "Click 'Go!' button to see constraint maps"))
    par(bty="n", mar=c(4.1,4.1,4.1,0.1), mfrow=c(3,1))
    
    removed_access = signif((1 - (ncell_nw_idn_ras - freq(x=accessedit(), value=NA)) / 
                               ncell_non_na_nw_idn_ras)*100, 4)
    plot(nw_idn_ras, col=excluded_col, legend=FALSE, xlab="", ylab="", 
         xaxt="n", yaxt="n",
         main=paste0("Accessibility: ", removed_access, "%"),
         cex.main=2)
    plot(accessedit(), add=TRUE, col=lighter_purps(100))
    legend("topright", "Excluded sites", fill=excluded_col, bty="n", cex=1.5)
    
    removed_forest = signif((1 - (ncell_nw_idn_ras - freq(x=forestedit(), value=NA)) / 
                               ncell_non_na_nw_idn_ras)*100, 4)
    plot(nw_idn_ras, col=excluded_col, legend=FALSE, xlab="", ylab="",
         xaxt="n", yaxt="n", cex.main=2,
         main=paste0("Forest Cover: ", removed_forest, "%"))
    plot(forestedit(), add=TRUE, col=lighter_purps(100))
    
    if (length(overlay_ras$catch_polys) > 0){
      catches_as_spat_polys = do.call(bind, overlay_ras$catch_polys)
      removed_dist = signif((1 - (ncell_nw_idn_ras - freq(x=mask(nw_idn_ras,
                                                                 catches_as_spat_polys,
                                                                 inverse=TRUE), value=NA)) /
                               ncell_non_na_nw_idn_ras)*100, 4)
    } else {
      removed_dist = 0
    }
    plot(nw_idn_ras, col=excluded_col, legend=FALSE, xlab="", ylab="",
         xaxt="n", yaxt="n", cex.main=2,
         main=paste0("Catchment Exclusion: ", removed_dist,  "%"))
    tmp = overlay_ras$r
    values(tmp)[!is.na(values(tmp))] = 1
    plot(overlay_ras$r, add=TRUE, col=purps(100)[50], legend=FALSE)
    # check this bit is operational
    if (length(overlay_ras$catch_polys) > 0){ 
      plot(catches_as_spat_polys, add=TRUE, border=excluded_point)
    }
  }, height=1000, width=1000)
})


