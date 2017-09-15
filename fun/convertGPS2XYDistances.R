convertGPS2XYDistances <- function(gps_lon, gps_lat, row4origin, showplot = F, lim_x = c(-100, 100), lim_y = c(-100, 100)) {
  
  outputFunProc(R)
  
  ## Determine origin
  ## Depending on setting (1 = real origin, 501 = critdist)
  origin <- data.frame(gps_lon = gps_lon[row4origin], 
                       gps_lat = gps_lat[row4origin])
  
  ## Create data copy
  #dat2proc.gps_med.conv <- dat2proc.gps_med
  dat <- data.frame(gps_lon = gps_lon, 
                    gps_lat = gps_lat)
  
  ## Conversion
  dat <-
    dat %>%
    ## Compute distance from origin (critical distance) to each coordinate
    ## ... by keeping constant either lateral or longitudinal coordinate
    rowwise %>%
    #group_by(sxx_dist_m_rnd1) %>%
    mutate(gps_lon_conv = distm(origin, c(gps_lon, origin$gps_lat)),
           gps_lat_conv = distm(origin, c(origin$gps_lon, gps_lat))) %>%
    ## Convert to numeric data (was list before)
    mutate(gps_lon_conv = as.numeric(gps_lon_conv),
           gps_lat_conv = as.numeric(gps_lat_conv)) %>%
    ## Correct distances by taking origin into account
    mutate(gps_lon_conv =
             ifelse(gps_lon < origin$gps_lon,
                    gps_lon_conv * -1,
                    gps_lon_conv),
           gps_lat_conv =
             ifelse(gps_lat < origin$gps_lat,
                    gps_lat_conv * -1,
                    gps_lat_conv)) %>% 
    ## Remove original data
    mutate(gps_lon = NULL,
           gps_lat = NULL) %>% 
    data.frame()
  
  if (showplot == T) {
    catWithSepLine("Visualising: GPS path in metres")
    
    plot(dat$gps_lon_conv,
         dat$gps_lat_conv,
         type = "l", 
         xlim = lim_x, 
         ylim = lim_y)
  }
  return(dat)
}
