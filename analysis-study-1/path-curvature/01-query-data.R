
# Query settings ----------------------------------------------------------

## Data
sett_query <- c()
sett_query$db_name <- "URBAN-MV-VIE_UniBw_Study-1"
sett_query$db_conn_name <- dbFindConnObj(sett_query$db_name, output = F)
sett_query$src_name_prefix <- "t_adtf"
sett_query$src_name_suffix <- "full_aggr_dti_rnd1"
sett_query$df_name_prefix <- "study1"

## Filter
sett_query$pxx   <- c(4)
sett_query$round <- c("intro", "normal", "stress")
sett_query$subject <- c(1:31)
sett_query$col_name_am <- "dti_m_rnd1"
sett_query$am_limit1 <- -50
sett_query$am_limit2 <- 25
sett_query$am_buffer <- 50

## Variables 
sett_query$col_names_session <-
  c("subject_id",
    "round_id",
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



# Data query --------------------------------------------------------------

dbGetQuery_pxxBatch(sett_query$db_conn_name, sett_query)