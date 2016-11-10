computeSmoothGPSMedian <- function(sxx_nr, set4proc, pause = F) {
  
  ## Compute GPS path median
  dat4med <- 
    get(set4proc$objname, envir = .GlobalEnv) %>% 
    filter(sxx == sxx_nr) %>% 
    group_by_(.dots = lapply(set4proc$groupby, as.symbol)) %>%
    summarise(lon_med = median(gps_long),
              lat_med = median(gps_lat)) %>%
    data.frame()
  
  ## Extract lon and lat
  lon_med <- dat4med$lon_med
  lat_med <- dat4med$lat_med
  
  cat("\n")
  print(length(lon_med))
  cat("\n")
  print(length(lat_med))
  
  
  if (sxx_nr == 9) {
    ## See protoyping for explanation
    lon_med <- lon_med[224:length(lon_med)]
    #lat_med <- lat_med[227:length(lat_med)]
    lat_med <- lat_med[224:length(lat_med)]
  } 
  
  cat("\n")
  print(length(lon_med))
  cat("\n")
  print(length(lat_med))
  
  ## Load smoothing parameters from database
  par4loess <- 
    dbGetSrc("dbconn_study1", "t_sxx_gps2") %>% 
    filter(situation == sxx_nr) %>% 
    select(loess_span, loess_degree)
  set4proc$smooth_gps$loess_span <- par4loess$loess_span
  set4proc$smooth_gps$degree <- par4loess$loess_degree
  
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
  # lon_med_smooth <- predict(model_lon, c(1:length(dat4med$lon_med)) )
  # lat_med_smooth <- predict(model_lat, c(1:length(dat4med$lat_med)) )
  lon_med_smooth <- predict(model_lon, c(1:length(lon_med)) )
  lat_med_smooth <- predict(model_lat, c(1:length(lat_med)) )
  
  cat("\n")
  print(length(lon_med_smooth))
  cat("\n")
  print(length(lat_med_smooth))
  
  # if (!is.na(set4proc$rollmean_k)) {
  # #if (set4proc$sxx == 14) {
  #   lon_med_smooth <- rollmean(lon_med_smooth, set4proc$rollmean_k)
  #   lat_med_smooth <- rollmean(lat_med_smooth, set4proc$rollmean_k)
  #   lon_med_smooth <- c(rep(0, set4proc$rollmean_k/2), lon_med_smooth, rep(0, set4proc$rollmean_k/2))
  #   lat_med_smooth <- c(rep(0, set4proc$rollmean_k/2), lat_med_smooth, rep(0, set4proc$rollmean_k/2))
  # }
  
  ## Visualise smooth gps path 
  if(set4proc$plot == T) {
    catWSepLine("Visualising: (Loess) Smooth GPS Median")
    plot(dat4med$lon_med, dat4med$lat_med, 
         type = "l",
         lwd = 3,
         main = paste("Smooth GPS Median | Intersection #", sxx_nr))
    mtext(paste("Loess: ", 
                "span = ", set4proc$smooth_gps$loess_span, "; ",
                "degree = ", set4proc$smooth_gps$degree, 
                sep = ""))
    lines(lon_med_smooth, lat_med_smooth, col = "red", lwd = 2)
    legend("topleft", 
           c("Median", "Smoothed median"), 
           col = c("black", "red"),
           lty = 1)
    if(pause) pauseAndContinue()
  }
  
  # Visualise smooth deviations from median
  if (set4proc$plotdev) {
    if (sxx_nr != 9) { 
      catWSepLine("Visualising: Deviatons of smoothed model from GPS median")
      plot(abs(model_lon$residuals), abs(model_lat$residuals), 
           type = "l",
           main = paste("Deviatons of smoothed model from GPS median | Intersection #", sxx_nr),
           xlim = c(0, 0.00001),
           ylim = c(0, 0.00001))
    } else {
      par(mfrow = c(1, 2))
      plot(abs(model_lon$residuals), 
           type = "l",
           main = paste("Deviatons of smoothed model from GPS median (long.) | Intersection #", sxx_nr),
           ylim = c(0, 0.00001))
      plot(abs(model_lat$residuals), 
           type = "l",
           main = paste("Deviatons of smoothed model from GPS median (lat.) | Intersection #", sxx_nr),
           ylim = c(0, 0.00001))
      par(mfrow = c(1, 1))
    }
    
    if(pause) pauseAndContinue()
  }
  
  # Visualise results on map image
  if (set4proc$plotmap) {
    catWSepLine("Visualising: Smooth GPS Median on map data")
    mapdat <- getMapImage(sxx_nr, zoom = set4proc$plotmap_zoom) 
    plotdat <- 
      ggmap(mapdat) + 
      geom_path(data = dat4med,
      #geom_path(data = data.frame(lon_med = lon_med, lat_med = lat_med),
                aes(x = lon_med,
                    y = lat_med,
                    colour = "data"),
                size = 3) +
      geom_path(data = data.frame(lon = lon_med_smooth,
                                  lat = lat_med_smooth),
                aes(x = lon,
                    y = lat,
                    colour = "median"),
                size = 1) +
      scale_colour_manual(name = "GPS path",
                          values = c(data = "yellow", median = "red")) + 
    theme(legend.justification = c(0,1),
          legend.position = c(0, 1))
    plot(plotdat)
  }
  
  
  return(data.frame(
    #gps_lon_med = lon_med, 
    #gps_lat_med = lat_med, 
    gps_lon = lon_med_smooth, 
    gps_lat = lat_med_smooth))
}
