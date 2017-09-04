
# Preparatory settings ----------------------------------------------------

set4proc <- c()
#set4proc$pxx <- c(1:18,99)
#set4proc$pxx <- c(24)
set4proc$pxx <- c(1:6)
#set4proc$pxx <- 1
set4proc$dbconn <- "db_conn_9"
#set4proc$name4src <- "t_pxx_gps2"
set4proc$name4src <- "t_gps_positions"
set4proc$zoom <- 19
set4proc$scale <- 2
set4proc$maptype <- "satellite"
set4proc$source <- "google"



# Load gps reference points -----------------------------------------------

assign(set4proc$name4src,
       dbGetSrc(set4proc$dbconn, set4proc$name4src),
       .GlobalEnv)




# Get map images ----------------------------------------------------------

for(pxx_temp in set4proc$pxx) {
  
  dat2proc <- get(set4proc$name4src)
  #dat2proc <- dat2proc %>% filter(situation == pxx)
  dat2proc <- dat2proc %>% filter(position_id == pxx_temp)
  lat <- dat2proc$lat
  lon <- dat2proc$lon
  
  map <- get_map(location = c(lon, lat),
                 zoom = set4proc$zoom,
                 scale = set4proc$scale,
                 maptype = set4proc$maptype,
                 source = set4proc$source)
  
  filepath <- file.path("resources", "study-2", "map-images")
  dir.create(filepath, recursive = T, showWarnings = F)
  filename <- paste(sprintf("s%02d", pxx_temp),
                    set4proc$source,
                    set4proc$maptype,
                    paste("scale", set4proc$scale, sep = ""),
                    paste("zoom", set4proc$zoom, sep = ""),
                    sep = "_")
  
  save(map, 
       file = file.path(filepath, paste(filename, ".RData", sep = "")), 
       envir = .GlobalEnv)
  
  #mapplot <-  ggmap(map, extent = "device")
  mapplot <-  ggmap(map)
  
  ggsave(file = file.path(filepath, paste(filename, ".png", sep = "")),
         mapplot,
         width = 4.27, height = 4.27,
         dpi = 300)
}
