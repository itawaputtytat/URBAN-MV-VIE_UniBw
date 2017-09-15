writeSelfAsLog("sett_query")

# Query settings ----------------------------------------------------------

sett_query <- c()
sett_query$db_name <- "URBAN_MV_VIE_UniBw_Study1"
sett_query$db_conn_name <- dbFindConnObj(sett_query$db_name, output = F)
sett_query$src_name_prefix <- "t_adtf"
sett_query$src_name_suffix <- "full_dti_rnd1"
sett_query$df_name_prefix <- "study1"
sett_query$pxx   <- 4
sett_query$round <- c("intro", "normal", "stress")
sett_query$subject <- c(1:31)
sett_query$col_name_am_suffix <- "dti_m_rnd1"
sett_query$col_name_am <- paste_("pxx", sett_query$col_name_am_suffix)
sett_query$am_limit1 <- -75
sett_query$am_limit2 <- 75
sett_query$am_buffer <- 50
sett_query$col_names_session <-
  c("subject_id",
    "round_txt",
    "time_s",
    "dist_m",
    "gps_lat",
    "gps_lon")
sett_query$col_names_data <-
  c("speed_kmh",
    "acc_lon_ms2",
    "acc_lat_ms2",
    "acc_pedal_pos_perc",
    "brake_status",
    "brake_press_bar",
    "steer_angle_deg",
    "yaw_rate_degs")

sett_query$filter$sets <-
  list(
    list(sett_id_names$active$round, sett_query$round, "=", "OR"),
    list(sett_id_names$active$subject, sett_query$subject, "=", "OR"),
    list(sett_query$var_dist, sett_query$am_limit1 - sett_query$dist_buffer, ">="),
    list(sett_query$var_dist, sett_query$am_limit2 + sett_query$dist_buffer, "<=")
  )
sett_query$filter$bool_op_between <- c("OR")
sett_query$filter$bool_op_between <- c("AND")



# Data processing ---------------------------------------------------------

dbGetQuery_pxxBatch(sett_query$db_conn_name, sett_query, bind_rows = T)

test <- 
  get(sett_query$df_name) %>% 
  filter(passing == unique(get(sett_query$df_name)$passing)[2]) %>% 
  arrange(pxx_dti_m_rnd1)

map <- getMapImage(sett_query$pxx, zoom = 19)
ggmap(map) + 
#ggplot() + 
  geom_path(data = test,
            aes(x = gps_lon,
                y = gps_lat,
                group = passing),
            color = "yellow")




# study1_t_adtf_pxx_full_dti_rnd1 %>% 
#   filter(pxx_dti_m_rnd1 == min(pxx_dti_m_rnd1)) %>% 
#   arrange(desc(speed_kmh)) %>% 
#   head()
# 
# study1_t_adtf_pxx_full_dti_rnd1 %>% 
#   filter(passing == "p04_stress_s26") %>% 
#   summary()
# 
# ggplot() + 
#   geom_line(data = 
#               study1_t_adtf_pxx_full_dti_rnd1_intrpld %>% 
#               filter(passing == "p04_stress_s26"),
#             aes(x = time_s,
#                 y = speed_kmh,
#                 group = passing)) +
#   coord_cartesian(xlim = c(2330, 2343),
#                   ylim = c(0, 100))
# 
# 
# test <- 
#   dbGetQuery(db_conn_6,
#              "SELECT * FROM t_adtf_formatted 
#              WHERE subject_id = 26 AND time_s >= 2330 AND time_s <= 2343")
# 
# ggplot() + 
#   geom_line(data = test,
#             aes(x = time_s,
#                 y = speed_kmh)) +
#   coord_cartesian(xlim = c(2330, 2343),
#                   ylim = c(0, 100))
