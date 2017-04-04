## Procedure check for computing arrival measures

writeSelfAsLog("seq4query")



# Query settings ----------------------------------------------------------

set4query <- c()
set4query$src <- "t_adtf_dist_m_rnd1_full"
set4query$sxx   <- c(21)
set4query$round <- c("intro", "normal", "stress")
set4query$subject <- c(1:31)
set4query$distvar <- "dist_m_rnd1"
set4query$dist1 <- -50
set4query$dist2 <- 25
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

dbGetQuery_batch("dbconn_study1", set4query, rb = T)