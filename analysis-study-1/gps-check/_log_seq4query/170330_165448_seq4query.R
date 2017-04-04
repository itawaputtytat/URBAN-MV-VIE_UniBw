## Procedure check for computing arrival measures

writeSelfAsLog("seq4query")



# Query settings ----------------------------------------------------------

set4query <- c()
set4query$src <- "t_adtf_dist_m_rnd1_full"
set4query$sxx   <- c(21)
set4query$round <- c("intro", "normal", "stress")
set4query$subject <- c(6:11)
set4query$distvar <- "dist_m_rnd1"
set4query$dist1 <- -5
set4query$dist2 <- 20
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
  dat2vis%>% 
  filter(s21_dist_m_rnd1 == 0)

plotdat <- 
  ggmap(map) + 
  #ggplot() +
  geom_point(data = dat2vis, 
             aes(x = gps_lon,
                 y = gps_lat,
                 colour = as.factor(subject_id))) + 
  geom_line(data = dat2vis, 
            aes(x = gps_lon,
                y = gps_lat,
                colour = as.factor(subject_id)),
            size = 0.5) + 
  facet_grid(.~round_txt)

plot(plotdat)
