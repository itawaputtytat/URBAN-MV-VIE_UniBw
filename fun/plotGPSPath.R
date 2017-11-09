plotGPSPath <- function(dat, 
                        position_id, 
                        col_name_case, 
                        case_filter,
                        col_names_gps = c("gps_lon", "gps_lat"),
                        zoom = 19, 
                        ccolor = "yellow") {
  
  map <- getMapImage(position_id, zoom = zoom)
  
  row_finder <- which(dat[, col_name_case] %in% case_filter)
  dat <- dat[row_finder, ]
  
  plot_gps_path <- 
    ggmap(map) + 
    geom_path(data = dat,
              aes_string(x = col_names_gps[1],
                         y = col_names_gps[2],
                         group = col_name_case,
                         color = col_name_case))
  
  return(plot_gps_path)
  
}