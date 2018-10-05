
# Settings ----------------------------------------------------------------

sett_curv <- c()
#sett_curv$df_name <- "dat_study2_t_adtf_pxx_full_aggr_dti_rnd1_intrpld_cut"
sett_curv$df_name <- "dat_study1_t_adtf_pxx_full_aggr_dti_rnd1_intrpld_cut"
sett_curv$col_names$pxx <- "pxx"
sett_curv$col_names$id <- "passing"
sett_curv$col_names$am <- "dti_m_rnd1"
sett_curv$col_names$time <- "time_s"
sett_curv$col_names$gps_lon <- "gps_lon"
sett_curv$col_names$gps_lat <- "gps_lat"

test <- 
  computeAMToSteerMax(
  get(sett_curv$df_name), 
  col_name_am = sett_curv$col_names$am,
  col_name_steer = "steering_wheel_angle_deg",
  col_names_group = sett_curv$col_names$id,
  col_name_target = paste_(sett_curv$col_names$am, "steer"))

assign(sett_curv$df_name, test)

## Filter
sett_curv$filters$pxx <- 1

## Test case for visualization
sett_curv$case <- paste_(sprintf("p%02d", sett_curv$filters$pxx), "s03_intro")

## Database
sett_curv$db$db_name <- "URBAN-MV-VIE_UniBw_Study-1"
sett_curv$db$conn_name <- dbFindConnObj(sett_curv$db$db_name, output = F)

## Create table name for data base
sett_curv$db$target_name$prefix <- "t_curv"
sett_curv$db$target_name$pxx <- 
  sprintf("p%02d", sett_curv$filters$pxx)
sett_curv$db$target_name$am <- 
  ifelse(grepl("dti", sett_curv$col_names$am), 
         "aggr_dti",
         "aggr_tti")
sett_curv$db$target_name$final <- 
  paste_(sett_curv$db$target_name$prefix,
         sett_curv$db$target_name$pxx,
         sett_curv$db$target_name$am)

## Plot
sett_curv$plot$show_plot <- TRUE
#sett_curv$plot$col_names$am <- "dti_m_rnd1_steer"
sett_curv$plot$col_names$am <- "dti_m_rnd1"
sett_curv$plot$col_names$curv <- "curv"


sett_curv$col_names$am <- sett_curv$plot$col_names$am 

# Preprocessing -----------------------------------------------------------

dat_curv <- 
  get(sett_curv$df_name) %>% 
  filter_(paste(sett_curv$col_names$pxx, "==", sett_curv$filter$pxx))



# Compute path median -----------------------------------------------------

dat_curv_median <- 
  dat_curv %>% 
  group_by_(.dots = sett_curv$col_names$am) %>% 
  summarize(gps_lon = median(gps_lon),
            gps_lat = median(gps_lat))

# ggplot() +
#   geom_path(data = dat_curv,
#             aes_string(x = sett_curv$col_names$gps_lon,
#                        y = sett_curv$col_names$gps_lat,
#                        group = sett_curv$col_names$id),
#             alpha = 0.25) +
#   geom_path(data = dat_curv_median,
#             aes_string(x = sett_curv$col_names$gps_lon,
#                        y = sett_curv$col_names$gps_lat),
#             color = "red")



# Calculate curvature -----------------------------------------------------

## Convert GPS into XY coordinates
dat_curv_median_xy <- 
  convertGPS2XYDistances(dat_curv_median$gps_lon,
                         dat_curv_median$gps_lat)

## Add AM information
dat_curv_median_xy <- 
  cbind(dat_curv_median[, sett_curv$col_names$am],
        dat_curv_median_xy)

## Compute radius
dat_curv_median_xy$r <- 
  computePathRadius(dat_curv_median_xy, 
                    col_name_x = "x", 
                    col_name_y = "y",
                    step = 50)

dat_curv_median_xy <- 
  dat_curv_median_xy %>% 
  filter_(paste(sett_curv$col_names$am, ">=", -100, "&",
                sett_curv$col_names$am, "<=", 50))

## Convert radius to curvature
dat_curv_median_xy <- 
  computePathCurvature(dat_curv_median_xy,
                       col_name_r = "r",
                       threshold_r = 10^3,
                       roll_avg_k = 150)

if (sett_curv$plot$show_plot) {
  
  ## Visualize smoothed radius
  plot(dat_curv_median_xy$r_restr, type = "l", ylim = c(0, 10^3))
  lines(dat_curv_median_xy$r_restr_smooth, col = "red", lwd = 3)
  
  ## Visualize points with greatest curvature values
  plot(ggplot() +
         geom_point(data = dat_curv_median_xy,
                    aes_string(x = "x",
                               y = "y",
                               size = sett_curv$plot$col_names$curv),
                    alpha = 0.01))
  
}

# createCircleDat <- function(x = 0, y = 0, r = 1, n_points=100) {
#     t <- seq(0 , 2*pi, length.out = n_points)
#     x <- x + r * cos(t)
#     y <- y + r * sin(t)
#     return (data.frame(x = x, y = y))
# }



# Write curvature to database ---------------------------------------------

# dbWriteTable(get(sett_curv$db$conn_name),
#              sett_curv$db$target_name$final,
#              dat_curv_median_xy,
#              row.names = FALSE,
#              overwrite = TRUE)



# Compute DSM -------------------------------------------------------------

if (sett_curv$plot$show_plot) {
  
  ## Limit AM range
  dat_curv_median_xy_restr <- 
    dat_curv_median_xy %>% 
    filter_(paste(sett_curv$col_names$am, ">=", -100, "&",
                  sett_curv$col_names$am, "<=", 50))
  
  ## Compute DSM
  dat_curv_dsm <- 
    computeDSMFromPathCurvature(
      dat_curv_median_xy_restr,
      col_name_am = sett_curv$col_names$am,
      col_name_curv = "curv_norm",
      u_max = c(48/3.6, 54/3.6, 60/3.6),
      acc_lat_max = c(2, 2.75, 3.5),
      gradients = c(0.15, 0.2, 0.25))
  
}



# Visualization of DSM ----------------------------------------------------

if (sett_curv$plot$show_plot) {
  
  ## Reshape to long data
  dat_curv_dsm_long <- 
    dat_curv_dsm %>% 
    gather(dsm, value, -one_of(sett_curv$col_names$am, "curv_norm", "s_diff"))
  
  ## Load test case
  dat_curv_test <- 
    dat_curv %>% 
    filter_(paste0(sett_curv$col_names$id, "==", "\"", sett_curv$case, "\"")) %>% 
    filter_(paste(sett_curv$col_names$am, ">=", -100, "&",
                  sett_curv$col_names$am, "<=", 50))
  
  ggplot() +
    geom_line(data = dat_curv_test,
              aes_string(x = sett_curv$plot$col_names$am,
                         y = "speed_ms")) +
    geom_line(data = dat_curv_dsm_long %>% filter(!grepl("gr", dsm)),
              aes_string(x = sett_curv$plot$col_names$am,
                         y = "value",
                         group = "dsm",
                         color = "dsm"),
              size = 1,
              linetype = "dotted") +
    geom_line(data = dat_curv_dsm_long %>% filter(grepl("gr", dsm)),
              aes_string(x = sett_curv$plot$col_names$am,
                         y = "value",
                         group = "dsm",
                         color = "dsm"),
              size = 1) +
    scale_color_brewer(palette = "Set1") +
    coord_cartesian(ylim = c(0, 20))
  
}

