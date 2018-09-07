
# Query settings ----------------------------------------------------------

## Data
sett_query <- c()
sett_query$db_name <- "URBAN-MV-VIE_UniBw_Study-2"
sett_query$db_conn_name <- dbFindConnObj(sett_query$db_name, output = F)
sett_query$src_name_prefix <- "t_adtf"
sett_query$src_name_suffix <- "full_aggr_tti_rnd1"
#sett_query$src_name_suffix <- "full_aggr_tti_rnd1"
sett_query$df_name_prefix <- "study2"

## Filter
#sett_query$pxx   <- c(1,2,3,4,5,6)
sett_query$pxx   <- 1
sett_query$round <- c("t1_v50", "t2_v50", "t1_v70", "t2_v70")
sett_query$subject <- c(1:40)
sett_query$col_name_am <- "tti_s_rnd1"
sett_query$am_limit1 <- -50
sett_query$am_limit2 <- 350
sett_query$am_buffer <- 50

sett_query$col_names_session <-
  c("subject_id",
    "round_id",
    "condition_speed",
    "time_s",
    "driven_distance_m",
    "gps_lat",
    "gps_lon")

sett_query$col_names_data <-
  c("speed_kmh",
    "acc_lon_ms2",
    "acc_lat_ms2",
    "acc_pedal_position_perc",
    "brake_status",
    "brake_press_bar",
    "steering_wheel_angle_deg",
    "yaw_rate_degs")



# Query data --------------------------------------------------------------

dbGetQuery_pxxBatch(sett_query$db_conn_name, sett_query)
