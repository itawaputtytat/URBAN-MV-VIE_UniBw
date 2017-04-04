## Procedure check for computing arrival measures

writeSelfAsLog("seq4query")



# Query settings ----------------------------------------------------------

set4query <- c()
set4query$src <- "t_adtf_dist_m_rnd1_full"
set4query$sxx   <- c(21)
set4query$round <- c("intro", "normal", "stress")
set4query$subject <- c(1)
set4query$distvar <- "dist_m_rnd1"
set4query$dist1 <- -5
set4query$dist2 <- 5
set4query$distbuffer <- 50
set4query$save2df_prefix <- "adtf"
set4query$var_session <-
  c("subject_id",
    "round_txt",
    "time_s",
    "gps_lat",
    "gps_lon")
set4query$var_sxx <-
  c("_dist_s",
    "_dist_m_rnd1")
set4query$var_data <-
  c("speed_kmh",
    "acclat_ms2",
    "acclon_ms2",
    "steerangle_deg")



# Data processing ---------------------------------------------------------

dbGetQuery_batch(dbconn_study1, set4query, rb = T)



# Visualise minimimal distances to GPS reference point --------------------

map <- getMapImage(set4query$sxx, zoom = 19)

dat2vis <- 
  t_adtf_dist_m_rnd1_full %>% 
  group_by(passing) %>% 
  #filter(s21_dist_m_rnd1 == min(abs(s21_dist_m_rnd1))) %>% 
  data.frame()

plotdat <- 
  ggmap(map) + 
  geom_point(data = dat2vis, 
             aes(x = gps_lon,
                 y = gps_lat))

plot(plotdat)