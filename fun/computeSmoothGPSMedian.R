computeSmoothGPSMedian <- function(dat2proc, sxx_nr, set4proc, pause = F) {
  
  outputFunProc(R)
  
  ## Remove cases with GPS anomalies
  ## (might have incorrect GPS interpolation)
  cases2corr <-
    dbGetSrc("dbconn_study1", "v_steerangle_pos_correction") %>%
    filter(grepl("correction", consequence))
  
  cases2corr <-
    paste(paste(sprintf("s%02d", cases2corr$sxx),
                cases2corr$round_txt,
                sprintf("subject%02d", cases2corr$subid), sep = "_"))
  
  dat4med <-
    dat2proc %>% 
    filter(sxx == sxx_nr) %>% 
    filter(!passing %in% cases2corr)
    
  ## Filter data in case of sxx == 9
  if (sxx_nr == 9)
    dat4med <-
      dat4med %>%
    filter(sxx_dist_m_rnd1 >= -25) ## -25 explored using visualisation
  
  ## Compute GPS path median
  dat4med <- 
    dat4med %>% 
    group_by_(.dots = lapply(set4proc$groupby, as.symbol)) %>%
    summarise(lon_med = median(gps_lon),
              lat_med = median(gps_lat)) %>%
    data.frame()
  
  ## Extract lon and lat
  lon_med <- dat4med$lon_med
  lat_med <- dat4med$lat_med
  
  ## Backup filter
  # if (sxx_nr == 9) {
  # #lon_med <- lon_med[224:length(lon_med)]
  # #lat_med <- lat_med[224:length(lat_med)]
  #   rowfinder <- which(lat_med > 48.1033) #48.1025
  #   lon_med <- lon_med[rowfinder]
  #   lat_med <- lat_med[rowfinder]
  # }

  ## Load smoothing parameters from database
  par4loess <- 
    dbGetSrc("dbconn_study1", "t_sxx_gps2") %>% 
    filter(situation == sxx_nr) %>% 
    select(loess_span, loess_degree)
  set4proc$smooth_gps$loess_span <- par4loess$loess_span
  set4proc$smooth_gps$degree <- par4loess$loess_degree
  
  model_lon <- smoothWithLoess(dat4model_lon_med, 
                               set4proc$smooth_gps$loess_span, 
                               set4proc$smooth_gps$degree)
  model_lat <- smoothWithLoess(dat4model_lat_med, 
                               set4proc$smooth_gps$loess_span, 
                               set4proc$smooth_gps$degree)
  lon_med_smooth <- model_lon$fitted
  lat_med_smooth <- model_lat$fitted
  
  
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
    gps_lon = lon_med_smooth, 
    gps_lat = lat_med_smooth))
}
