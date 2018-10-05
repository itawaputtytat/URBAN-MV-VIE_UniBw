convertGPS2XYDistances <- function(gps_lon, gps_lat, row_nr_origin = 1) {
  
  ## Determine origin
  ## Depending on setting (1 = real origin, 501 = critdist)
  origin <- data.frame(gps_lon = gps_lon[row_nr_origin], 
                       gps_lat = gps_lat[row_nr_origin])
  
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
    mutate(x = distm(origin, c(gps_lon, origin$gps_lat)),
           y = distm(origin, c(origin$gps_lon, gps_lat))) %>%
    ## Convert to numeric data (was list before)
    mutate(x = as.numeric(x),
           y = as.numeric(y)) %>%
    ## Correct distances by taking origin into account
    mutate(x =
             ifelse(gps_lon < origin$gps_lon,
                    x * -1,
                    x),
           y =
             ifelse(gps_lat < origin$gps_lat,
                    y * -1,
                    y)) %>% 
    ## Remove original data
    mutate(gps_lon = NULL,
           gps_lat = NULL) %>% 
    data.frame()

  return(dat)
}
