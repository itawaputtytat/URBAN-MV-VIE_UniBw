
# Load template -----------------------------------------------------------

replayPlot(plot_template4sim)



# History of simulated speed profiles -------------------------------------

if (!sett_plot$pred_is_single != 0 & sett_plot$plot_sim_tails_history) {
  
  if (!sett_pred$collect_sim_tails) {
    outputString("Error: Collection of simulation tails is not activated!")
  }
  
  invisible(
    
    lapply(seq_along(dat_sim_tails_coll), function(x) {
      
      dat <- dat_sim_tails_coll[[x]][-1, ]
      name <- names(dat_sim_tails_coll)[[x]]
      
      if (grepl("j1", name))
      { col <- "#6FCDDD65"; shape = 17 } # cyan
      
      if (grepl("j2", name))
      { col <- "#FFA50065"; shape = 18 } # orange
      
      if (grepl("j3", name))
      { col <- "#ED212565"; shape = 16 } # red
      
      if (grepl("j4", name))
      { col <- "#B9539F65"; shape = 15 } # magenta
      
      lines(lwd = 0.75,
      #points(pch = ".",
            x = dat$dist_m, 
            y = dat$speed_ms,
            col = col)
    })
    
  )
}



# Rectangle for time lag --------------------------------------------------

## Rectangle
rect(xleft = sett_sim_temp$am1, 
     xright = sett_sim_temp$am2,
     ybottom = sett_plot$ymin, 
     ytop = sett_plot$ymax,
     col = "#00EE0022")

## Left and right border lines
abline(v = sett_sim_temp$am2, 
       col = "green4")
abline(v = sett_sim_temp$am1, 
       col = "green4")

## Arrow to the left
arrows(x0 = sett_sim_temp$am2, 
       x1 = sett_sim_temp$am1,
       y0 = sett_plot$ymax - 2.5,
       lwd = 2,
       angle = 40,
       col = "green4",
       length = 0.15,
       xpd = T)

## Rectangle and text for timelag
rect(xleft = (sett_sim_temp$am2 + sett_sim_temp$am1)/2 - 3, 
     xright = (sett_sim_temp$am2 + sett_sim_temp$am1)/2 + 3,
     ybottom = sett_plot$ymax - 4.5, ytop = sett_plot$ymax - 3.5,
     col = "#00EE0099")
text(x = (sett_sim_temp$am2 + sett_sim_temp$am1)/2,
     y =  sett_plot$ymax - 4,
     labels = paste(sett_sim$time_lag_s, "s"))

## Rectangle and text for current position
rect(xleft = (sett_sim_temp$am2 + sett_sim_temp$am1)/2 - 3, 
     xright = (sett_sim_temp$am2 + sett_sim_temp$am1)/2 + 3,
     ybottom = sett_plot$ymax - 1.5, ytop = sett_plot$ymax - 0.5,
     col = "white")
text(x = (sett_sim_temp$am2 + sett_sim_temp$am1)/2,
     y =  sett_plot$ymax - 1,
     labels = paste(round(sett_sim_temp$am2, 1), "m"))



# ## Current simulated speed profiles -------------------------------------

invisible(
  
  lapply(seq_along(dat_sim), function(x) {
    
    name <- names(dat_sim)[[x]]
    
    if (grepl("j1", name)) { 
      col <- "#6FCDDD" # cyan
      shape = 17 } 
    
    if (grepl("j2", name)) { 
      col <- "orange" # orange
      shape = 18 } 
    
    if (grepl("j3", name)) { 
      col <- "#ED2125" # red
      shape = 16 
    } 
    
    if (grepl("j4", name)) {
      col <- "#B9539F" # magenta
      shape = 15 
    } 
    
    lines(x = dat_sim[[x]]$dist_m,
          y = dat_sim[[x]]$speed_ms,
          col = col, 
          lwd = 1)
    
    points(x = tail(dat_sim[[x]]$dist_m),
           y = tail(dat_sim[[x]]$speed_ms),
           col = col, pch = shape, cex = 1.5)
  })
  
)
