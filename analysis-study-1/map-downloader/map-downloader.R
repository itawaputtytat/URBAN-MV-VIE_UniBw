
# Preparatory settings ----------------------------------------------------

set4proc <- c()
set4proc$sxx <- c(1:18,99)
#set4proc$sxx <- 1
set4proc$dbconn <- "dbconn_study1"
set4proc$name4src <- "t_sxx_gps2"
set4proc$zoom <- 18
set4proc$scale <- 2
set4proc$maptype <- "satellite"
set4proc$source <- "google"



# Load gps reference points -----------------------------------------------

assign(set4proc$name4src,
       dbGetSrc(set4proc$dbconn, set4proc$name4src),
       .GlobalEnv)




# Get map images ----------------------------------------------------------

for(sxx in set4proc$sxx) {
  
  dat2proc <- get(set4proc$name4src)
  dat2proc <- dat2proc %>% filter(situation == sxx)
  lat <- dat2proc$situation_lat
  lon <- dat2proc$situation_long
  
  map <- get_map(location = c(lon, lat),
                 zoom = set4proc$zoom,
                 scale = set4proc$scale,
                 maptype = set4proc$maptype,
                 source = set4proc$source)
  
  filepath <- file.path("ressources", "study1", "map-images")
  dir.create(filepath, recursive = T, showWarnings = F)
  filename <- paste(sprintf("s%02d", sxx),
                    set4proc$source,
                    set4proc$maptype,
                    paste("scale", set4proc$scale, sep = ""),
                    paste("zoom", set4proc$zoom, sep = ""),
                    sep = "_")
  
  save(map, 
       file = file.path(filepath, paste(filename, ".RData", sep = "")), 
       envir = .GlobalEnv)
  
  mapplot <-  ggmap(map, extent = "device")
  
  ggsave(file = file.path(filepath, paste(filename, ".png", sep = "")),
         mapplot,
         width = 4.27, height = 4.27,
         dpi = 300)
}
