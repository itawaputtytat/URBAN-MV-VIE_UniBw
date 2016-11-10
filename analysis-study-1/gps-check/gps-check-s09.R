
# Explore raw data --------------------------------------------------------

tobj <- dbListTables(dbconn_study1)
tobj <- tobj[grep("IN_PJ", tobj)]

test <- 
  dbGetSrc("dbconn_study1", paste("\"", tobj[6] ,"\"", sep = "")) %>% 
  data.frame()

#test2 <- test[12000:25000, c("longitude", "latitude")]
test2 <- test[, c("longitude", "latitude")]
test2$longitude <- as.numeric(test2$longitude)
test2$latitude <- as.numeric(test2$latitude)
ggplot() + geom_path(data = test2, aes(x = longitude, y = latitude))



# Explore extracted database table ----------------------------------------

dat2proc <- 
  can_sxx_dist_m_rnd1_rb.intrpl.cut %>% 
  filter(sxx == 9) %>% filter(subid == 8)

ggplot() + 
  geom_path(data = dat2proc, aes(x = gps_long, y = gps_lat, group = subid)) + 
  facet_grid(.~round_txt) + 
  coord_cartesian(ylim = c(48.1, 48.105), xlim = c(11.649, 11.65))



# Explore complete merged database table ----------------------------------

list4subids <- 9

for (s in list4subids) {
  
  outputString(paste("Procelist4subidsing sxx:", s))
  
  dat2proc <- 
    dbGetQuery(dbconn_study1, 
               paste("SELECT * FROM t_can_full_aggr_dist_m_rnd1_max_dist2sxx_v2 WHERE subid =", s))
  
  #plot(dat2proc$gps_lat[dat2proc$gps_lat >= dat2proc$gps_lat[1]], type = "l")
  #plot(dat2proc$gps_lat[dat2proc$gps_long >= dat2proc$gps_long[1]], type = "l")
  plotdat <- ggplot() + 
    geom_path(data = dat2proc[dat2proc$gps_lat >= dat2proc$gps_lat[1], ], aes(x = gps_long, y = gps_lat, colour = round_txt)) + 
    #facet_grid(.~round_txt) + 
    ggtitle(paste("Subid:", s)) 
  plot(plotdat)
  
  pauseAndContinue()
}



# Explore distance related merged database table --------------------------

for (s in list4subids) {
  
  dat2proc_filter <- 
    dbGetQuery(dbconn_study1, paste("SELECT * FROM t_can_full_aggr_dist_m_rnd1_max_dist2sxx_v2 WHERE subid =", s, 
                                    "AND s09_dist_m_rnd1 > -200 AND s09_dist_m_rnd1 < 200"))
  
  # dat2proc_subid_filter <- 
  #   dat2proc_subid %>% 
  #   filter(s09_dist_m_rnd1 > -200 & s09_dist_m_rnd1 < 200)
  
  plotdat <- ggplot() + 
    geom_path(data = dat2proc_filter, aes(x = gps_long, y = gps_lat, colour = round_txt)) + 
    #facet_grid(.~round_txt) + 
    ggtitle(paste("Subid:", s)) 
  
  plot(plotdat)
  pauseAndContinue()
  
}