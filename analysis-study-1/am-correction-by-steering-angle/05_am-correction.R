
## Correct identified anomalies by moving am-related data

# Preparatory settings ----------------------------------------------------

sett_dat <- c()
sett_dat$df_name <- "dat_study1_t_adtf_pxx_full_aggr_dti_rnd1_intrpld"
sett_dat$col_name_am <- "dti_m_rnd1"
sett_dat$col_name_sa <- "steering_wheel_angle_deg"
sett_dat$col_name_position <- "pxx"
sett_dat$col_name_group <- "round_id"
sett_dat$col_name_case <- "passing"
sett_dat$position <- 18



# Identify cases ----------------------------------------------------------

dat_before <- 
  get(sett_dat$df_name) %>% 
  filter_(paste(sett_dat$col_name_position, "==", sett_dat$position))

dat_cases <- suppressMessages(
  identifySteerAngleOutliers(dat_before,
                             sett_dat$col_name_am,
                             sett_dat$col_name_case,
                             sett_dat$col_name_group,
                             sett_dat$col_name_sa,
                             return_cases_only = T)
)



# Correct cases -----------------------------------------------------------

dat_after <- 
  correctPositionAnomalies(dat_before,
                            db_conn_name = sett_query$db_conn_name,
                            save_global = F)



# Visualize corrected cases -----------------------------------------------

if (length(dat_cases) != 0) {
  
  plot_after <- 
    ggplot() + 
    geom_line(data = dat_after,
              aes_string(x = sett_dat$col_name_am,
                         y = sett_dat$col_name_sa,
                         group = sett_dat$col_name_case)) + 
    geom_line(data = dat_before %>% 
                filter_(paste(sett_dat$col_name_case, 
                              "%in%", 
                              quote(dat_cases), 
                              collapse = "|")),
              aes_string(x = sett_dat$col_name_am,
                         y = sett_dat$col_name_sa,
                         group = sett_dat$col_name_case),
              col = "red",
              size = 1) + 
    geom_line(data = dat_after %>% 
                filter_(paste(sett_dat$col_name_case, 
                              "%in%", 
                              quote(dat_cases), 
                              collapse = "|")),
              aes_string(x = sett_dat$col_name_am,
                         y = sett_dat$col_name_sa,
                         group = sett_dat$col_name_case),
              col = "green4",
              size = 1) + 
    facet_grid(as.formula(paste(".~", sett_dat$col_name_group)))
  
  
  plot(plot_after)
  
}