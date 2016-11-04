computeSmoothGPSMedian <- function(situation, set4proc, pause = F) {
  
  ## Compute GPS path median
  dat4med <- 
    get(set4proc$objname, envir = .GlobalEnv) %>% 
    filter(sxx == situation) %>% 
    group_by_(.dots = lapply(set4proc$groupby, as.symbol)) %>%
    summarise(lon_med = median(gps_long),
              lat_med = median(gps_lat)) %>%
    data.frame()
  
  ## Extract lon and lat
  lon_med <- dat4med$lon_med
  lat_med <- dat4med$lat_med
  
  if(situation == 9) {
    ## See protoyping for explanation
    lon_med <- lon_med[224:length(lon_med)]
    lat_med <- lat_med[227:length(lat_med)]
  } 
  
  ## Compute model
  model_lon <-
    loess(lon_med ~ c(1:length(lon_med)),
          span = set4proc$smooth_gps$loess_span,
          degree = set4proc$smooth_gps$degree)
  
  model_lat <-
    loess(lat_med ~ c(1:length(lat_med)),
          span = set4proc$smooth_gps$loess_span,
          degree = set4proc$smooth_gps$degree)
  
  ## Predict values
  lon_med_smooth <- predict(model_lon, c(1:length(dat4med$lon_med)) )
  lat_med_smooth <- predict(model_lat, c(1:length(dat4med$lat_med)) )
  
  ## Visualise smooth gps path 
  if(set4proc$plot == T) {
    plot(dat4med$lon_med, dat4med$lat_med, type = "l")
    lines(lon_med_smooth, lat_med_smooth, col = "red", lwd = 2)
    legend("bottomright", 
           c("Median", "Smoothed median"), 
           col = c("black", "red"),
           lty = 2)
    
    if(pause) pauseAndContinue()
  }
  
  # Visualise smooth deviations from median
  if (set4proc$plotdev) {
    plot(abs(model_lon$residuals), abs(model_lat$residuals), type = "l",
         xlim = c(0, 0.00001),
         ylim = c(0, 0.00001))
    if(pause) pauseAndContinue()
  }
  
  # Visualise results on map image
  if (set4proc$plotmap) {
    
    mapdat <- getMapImage(situation, zoom = set4proc$plotmap_zoom) 
    
    plotdat <- 
      ggmap(mapdat) + 
      geom_path(data = data.frame(lon = lon_med_smooth, 
                                  lat = lat_med_smooth),
                aes(x = lon, 
                    y = lat),
                colour = "red",
                size = 1.5) +
      geom_path(data = dat4med,
                aes(x = lon_med, y = lat_med),
                colour = "yellow")
    plot(plotdat)
  }
  return(data.frame(
    #gps_lon_med = lon_med, 
    #gps_lat_med = lat_med, 
    lon_med_smooth, 
    lat_med_smooth))
}
