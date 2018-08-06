
# Query settings ----------------------------------------------------------

sett_query <- c()
sett_query$db_name <- "URBAN-MV-VIE_UniBw_Study-1"
sett_query$db_conn_name <- dbFindConnObj(sett_query$db_name, output = F)
sett_query$src_name_prefix <- "t_adtf"
sett_query$src_name_suffix <- "full"
sett_query$df_name_prefix <- "study1"
sett_query$pxx   <- c(6)
sett_query$round <- c("intro", "normal", "stress")
sett_query$subject <- c(1:31)
sett_query$col_name_am <- "dti_m"
sett_query$am_limit1 <- -25
sett_query$am_limit2 <- 25 
sett_query$am_buffer <- 0
sett_query$col_names_session <-
  c("subject_id",
    "round_id",
    "time_s",
    "driven_distance_m",
    "tta_s")

sett_query$col_names_data <-
  c("speed_kmh",
    "gps_lon",
    "gps_lat")

sett_query$filter$sets <-
  list(
    list(sett_id_names$active$round, sett_query$round, "=", "OR"),
    list(sett_id_names$active$subject, sett_query$subject, "=", "OR"),
    list(sett_query$col_name_am, 
         sett_query$am_limit1 - sett_query$dist_buffer, ">="),
    list(sett_query$col_name_am, 
         sett_query$am_limit2 + sett_query$dist_buffer, "<=")
  )
sett_query$filter$bool_op_between <- c("OR")
sett_query$filter$bool_op_between <- c("AND")



# Data processing ---------------------------------------------------------

dbGetQuery_pxxBatch(sett_query$db_conn_name, sett_query, bind_rows = T)
