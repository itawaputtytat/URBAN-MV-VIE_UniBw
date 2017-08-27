
# Log ---------------------------------------------------------------------

writeSelfAsLog("sett_query")



# Settings ----------------------------------------------------------------

sett_query <- c()
#sett_query$db_name <- "URBAN_MV_VIE_UniBw_Study1"
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

sett_query$filter$sets <-
  list(
    list(sett_id_names$active$round, sett_query$round, "=", "OR"),
    list(sett_id_names$active$subject, sett_query$subject, "=", "OR"),
    list(sett_query$var_dist, sett_query$dist1 - sett_query$dist_buffer, ">="),
    list(sett_query$var_dist, sett_query$dist2 + sett_query$dist_buffer, "<=")
  )
sett_query$filter$bool_op_between <- c("OR")
sett_query$filter$bool_op_between <- c("AND")



# Load data ---------------------------------------------------------------

#dbFindConnObj(sett_query$db_name, output = T)
dbGetQuery_sxxBatch(db_conn_8, sett_query, bind_rows = T)
