
## Correct identified anomalies by moving am-related data

# Preparatory settings ----------------------------------------------------

sett_dat <- c()
sett_dat$df_name <- "dat_study1_t_adtf_pxx_full_dti_rnd1_intrpld"
sett_dat$col_name_am <- "pxx_dti_m_rnd1"
sett_dat$col_name_sa <- "steer_angle_deg"
sett_dat$col_name_position <- "pxx"
sett_dat$col_name_group <- "round_txt"
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

print(dat_cases)


# Visualize cases ---------------------------------------------------------

plot_before <- 
  ggplot() + 
  geom_line(data = dat_before,
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
  facet_grid(.~round_txt)

plot(plot_before)



# Correct cases -----------------------------------------------------------

dat_after <- 
  correctPositionAnomamlies(dat_before,
                            db_conn_name = sett_query$db_conn_name,
                            save_global = F)



# Visualize corrected cases -----------------------------------------------

plot_after <- 
  ggplot() + 
  geom_line(data = dat_after,
            aes_string(x = sett_dat$col_name_am,
                       y = sett_dat$col_name_sa,
                       group = sett_dat$col_name_case)) + 
  geom_line(data = dat_after %>% 
              filter_(paste(sett_dat$col_name_case, 
                            "%in%", 
                            quote(dat_cases), 
                            collapse = "|")),
            aes_string(x = sett_dat$col_name_am,
                       y = sett_dat$col_name_sa,
                       group = sett_dat$col_name_case),
            col = "red",
            size = 1) + 
  facet_grid(.~round_txt)


plot(plot_after)
