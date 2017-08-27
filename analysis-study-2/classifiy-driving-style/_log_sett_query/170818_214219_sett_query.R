
# Log ---------------------------------------------------------------------

writeSelfAsLog("sett_query")



# Settings ----------------------------------------------------------------

sett_query <- c()
sett_query$db_name <- "URBAN_MV_VIE_UniBw_Study2"
sett_query$src <- "t_adtf_full_rnd1_full"
sett_query$pxx   <- c(1,2,3)
#sett_query$run <- 1
#sett_query$speed <- 50
sett_query$round_txt <- c("t1_50", "t2_50", "t1_70", "t2_70")
sett_query$subject <- c(1:40)
#sett_query$subject <- c(1:17, 19:31)
sett_query$var_dist <- "dist_m_rnd1"
sett_query$dist1 <- -100
sett_query$dist2 <- 200
sett_query$dist_buffer <- 50
sett_query$var_session <-
  c("subject_id",
    "round_txt",
    "time_s",
    "lat",
    "lon")
sett_query$var_pxx <-
  c("_dist_m_rnd1",
    "_dist_s")
sett_query$var_data <-
  c("speed_kmh",
    "acclon_ms2",
    "acclat_ms2",
    "accpedalpos_perc",
    "brakepress_status",
    "brakepress_bar",
    "steerangle_deg")

sett_query$filter$sets <-
  list(
    list(sett_id_names$active$round, sett_query$round, "=", "OR"),
    list(sett_id_names$active$subject, sett_query$subject, "=", "OR"),
    list(sett_query$var_dist, sett_query$dist1 - sett_query$dist_buffer, ">="),
    list(sett_query$var_dist, sett_query$dist2 + sett_query$dist_buffer, "<=")
  )
sett_query$filter$bool_op_between <- c("OR")
sett_query$filter$bool_op_between <- c("AND")



# Fetching data -----------------------------------------------------------

#dbFindConnObj(sett_query$db_name, output = T)
dbGetQuery_pxxBatch(db_conn_9, sett_query, bind_rows = T)
t_adtf_full_rnd1_full$brakepress_status <- as.logical(t_adtf_full_rnd1_full$brakepress_status)
intrpldf_batch4rb(t_adtf_full_rnd1_full, suffix = ".intrpl", outputFlag = T)
