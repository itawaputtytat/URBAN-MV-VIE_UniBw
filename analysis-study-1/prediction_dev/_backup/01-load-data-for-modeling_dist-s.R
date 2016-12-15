writeSelfAsLog("set4query")

# Query settings ----------------------------------------------------------

set4query <- c()
set4query$src <- "t_adtf_dist_s_rnd1_full"
#set4query$sxx   <- c(1:2, 4:18)
set4query$sxx   <- c(4)
set4query$round <- c("intro", "normal", "stress")
set4query$subject <- c(1:31)
#set4query$subject <- c(1:17, 19:31)
set4query$distvar <- "dist_m"
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
  c("_dist_m",
    "_dist_s_rnd1")
set4query$var_data <-
  c("speed_kmh",
    "acclon_ms2")



# Data processing ---------------------------------------------------------

dbGetQuery_batch("dbconn_study1", set4query, rb = T)
intrpldf_batch4rb(t_adtf_dist_s_rnd1_full, colname4ref = "sxx_dist_s_rnd1", suffix = ".intrpl", outputFlag = T)
corrPosAnom_batch4rb(t_adtf_dist_s_rnd1_full.intrpl)
cut2dist_batch4rb(t_adtf_dist_s_rnd1_full.intrpl, "sxx_dist_m", set4query$dist1 - 5, set4query$dist2)

t_adtf_dist_s_rnd1_full.intrpl.cut <- 
  addVar4PrecVeh(t_adtf_dist_s_rnd1_full.intrpl.cut)

t_adtf_dist_s_rnd1_full.intrpl.cut <- 
  addVar4Stopping(t_adtf_dist_s_rnd1_full.intrpl.cut)
