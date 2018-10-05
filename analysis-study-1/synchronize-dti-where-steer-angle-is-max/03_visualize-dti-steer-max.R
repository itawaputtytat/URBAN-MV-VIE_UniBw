
# Preparatory settings ----------------------------------------------------

sett_dat <- c()
sett_dat$df_name <- paste_(sett_query$df_name, "intrpld")
sett_dat$col_name_am <- "dti_m_rnd1"
sett_dat$col_name_position <- "pxx"
sett_dat$col_name_steer_angle <- "steering_wheel_angle_deg"


# Visualize original DTI --------------------------------------------------

dat_summary_original <- 
  get(sett_dat$df_name) %>% 
  group_by_(sett_dat$col_name_position, 
            sett_dat$col_name_am) %>% 
  summarize_(.dots = setNames(list(
    interp(~ median(v), 
           v = as.name(sett_dat$col_name_steer_angle))),
    sett_dat$col_name_steer_angle))

ggplot() + 
  geom_line(data = dat_summary_original,
            aes_string(x = sett_dat$col_name_am,
                       y = sett_dat$col_name_steer_angle,
                       group = sett_dat$col_name_position)) 


dat_summary_new_dti <- 
  get(sett_dat$df_name) %>% 
  group_by_(sett_dat$col_name_position, 
            paste_(sett_dat$col_name_am, "steer_max")) %>% 
  summarize_(.dots = setNames(list(
    interp(~ median(v), 
           v = as.name(sett_dat$col_name_steer_angle))),
    sett_dat$col_name_steer_angle))

ggplot() + 
  geom_line(data = dat_summary_new_dti,
            aes(x = pxx_dti_m_rnd1_steer_max,
                y = steer_angle_deg,
                group = pxx)) 

