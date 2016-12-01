writeSelfAsLog("seq4query")

# Query settings ----------------------------------------------------------

set4query <- c()
set4query$src <- "t_adtf_dist_s_rnd1_full"
#set4query$sxx   <- c(1:2, 4:18)
set4query$sxx   <- c(4)
set4query$round <- c("intro", "normal", "stress")
set4query$subject <- c(1:31)
set4query$distvar <- "dist_s_rnd1"
set4query$dist1 <- -50
set4query$dist2 <- 50
set4query$distbuffer <- 50
set4query$save2df_prefix <- "adtf"
set4query$var_session <-
  c("subject_id",
    "round_txt",
    "time_s",
    "gps_lat",
    "gps_lon")
set4query$var_sxx <-
  c("_dist_s_rnd1",
    "_dist_m")
set4query$var_data <-
  c("speed_kmh",
    "acclon_ms2")



# Data processing ---------------------------------------------------------

dbGetQuery_batch("dbconn_study1", set4query, rb = T)
intrpldf_batch4rb(adtf_sxx_dist_m_rnd1_rb, suffix = ".intrpl", outputFlag = T)
corrPosAnom_batch4rb(adtf_sxx_dist_m_rnd1_rb)
cut2dist_batch4rb(adtf_sxx_dist_m_rnd1_rb.intrpl, "sxx_dist_m_rnd1", set4query$dist1, set4query$dist2)
