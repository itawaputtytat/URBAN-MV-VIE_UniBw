writeSelfAsLog("sett_query")

sett_query <- c()
sett_query$db_name <- "URBAN_MV_VIE_UniBw_Study1"
sett_query$src <- "t_adtf_dist_m_rnd1_full"
#sett_query$sxx   <- c(1:2, 4:18)
sett_query$sxx   <- c(15)
sett_query$round <- c("intro", "normal", "stress")
sett_query$subject <- c(1:31)
#sett_query$subject <- c(1:17, 19:31)
sett_query$var_dist <- "dist_m_rnd1"
sett_query$dist1 <- -50
sett_query$dist2 <- 50
sett_query$dist_buffer <- 50
sett_query$df_name_prefix <- "adtf"
sett_query$var_session <-
  c("subject_id",
    "round_txt",
    "time_s",
    "gps_lat",
    "gps_lon")
sett_query$var_sxx <-
  c("_dist_m_rnd1",
    "_dist_s")
sett_query$var_data <-
  c("speed_kmh",
    "acclon_ms2")


dbGetQuery_sxxBatch(sett_query, bind_rows = F)
