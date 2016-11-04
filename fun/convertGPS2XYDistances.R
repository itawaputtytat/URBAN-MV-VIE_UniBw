convertGPS2XYDistances <- function(dat2proc, set4proc) {
  
  gps_lon <- dat2proc[, 1]
  gps_lat <- dat2proc[, 2]
  
  ## Determine origin
  ## Depending on setting (1 = real origin, 501 = critdist)
  origin <- data.frame(gps_lon = gps_lon[set4proc$row4origin], 
                       gps_lat = gps_lat[set4proc$row4origin])
  
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
  
  if (set4proc$plot == T)
    plot(dat$gps_lon_conv,
         dat$gps_lat_conv,
         type = "l", 
         xlim = set4proc$xlim, 
         ylim = set4proc$ylim)
  
  return(dat)
}
