writeSelfAsLog("seq4query")



# Query settings ----------------------------------------------------------

set4query <- c()
set4query$src <- "t_can_full_aggr_dist_m_rnd1_max_dist2sxx_v2"
set4query$sxx   <- c(4:10)
set4query$round <- c("intro", "normal", "stress")
set4query$subid <- c(1:31)
set4query$distvar <- "dist_m_rnd1"
set4query$dist1 <- -50
set4query$dist2 <- 50
set4query$distpuffer <- 50
set4query$save2df_prefix <- "can"
set4query$var_session <-
  c("subid",
    "round_txt",
    "time_s",
    "dist_m_rnd1",
    "gps_lat",
    "gps_long")
set4query$var_sxx <-
  c("_dist_s_rnd1",
    "_dist_m_rnd1")
set4query$var_data <-
  c("steerangle_deg",
    "speed_kmh")



# Data processing ---------------------------------------------------------

dbQueryLoop("dbconn_study1", set4query)
renameVar_sxx()
intrpldf_batch()
correctPositionAnomalies("can", "dist_m_rnd1")
cut2dist("can", "dist_m_rnd1", "sxx_dist_m_rnd1", set4query$dist1, set4query$dist2)
computeVar_dist2steermax("can", "dist_m_rnd1")
can_sxx_dist_m_rnd1.intrpl.cut.rb <- rbindPassings("can", "dist_m_rnd1.intrpl.cut")