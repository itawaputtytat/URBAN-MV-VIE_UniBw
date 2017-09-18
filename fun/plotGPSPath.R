plotGPSPath <- function(dat, pxx, passing, zoom = 19, color = "yellow") {
  map <- getMapImage(pxx, zoom = zoom)
  dat <- dat[dat$passing %in% passing, ]
  plot_gps_path <- 
    ggmap(map) + 
    geom_path(data = dat,
              aes(x = gps_lon,
                  y = gps_lat,
                  group = passing,
                  color = passing))
  return(plot_gps_path)
}