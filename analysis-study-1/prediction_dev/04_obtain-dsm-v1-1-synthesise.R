
# Preparatory settings ----------------------------------------------------

sett_synth <- c()
sett_synth$df_name <- paste_(sett_query$df_name, "intrpld", "cut")
sett_synth$col_name_am <- sett_query$col_name_am
sett_synth$col_name_time <- "time_s"
sett_synth$col_name_group <- "passing"
sett_synth$col_name_round <- "round_txt"
sett_synth$col_name_speed <- "speed_ms"
sett_synth$pxx <- sett_query$pxx
sett_synth$am_limit1 <- sett_query$am_limit1
sett_synth$am_limit2 <- sett_query$am_limit2
sett_synth$show_plot <- F
sett_synth$show_plot_by_subject <- 1

## Load parameters for IDM 
source("fun_Liebner_2013/settings/sett_idm.R")

## Additional parameters necesseray fpr IDM-splitting
## (e.g. section for pedestrian)
#sett_synth$am_limit <- 11 #old: 12; 11 shows better empirical desired velocites
#sett_synth$am_limit <- 3 #for left turning
sett_synth$am_limit <- 1 # p01
sett_synth$am_limit <- 15 # p04
# sett_synth$am_limit <- 25 # p06
# sett_synth$am_limit <- 20 # p07
# sett_synth$am_limit <- 40 # p15
sett_synth$am_limit <- 35 # p17



# Subset data -------------------------------------------------------------

dat_synth <- 
  get(sett_synth$df_name) %>% 
  filter(pxx %in% sett_synth$pxx) %>% 
  filter_(paste(sett_synth$col_name_am, ">=", sett_query$am_limit1, "&",
            sett_synth$col_name_am, "<=", sett_query$am_limit2)) %>% 
  filter(stopping == "no_stopping") %>% 
  #filter(preceded == 0) %>% 
  select_(sett_synth$col_name_am,
          sett_synth$col_name_group,
          sett_synth$col_name_round,
          sett_synth$col_name_time,
          sett_synth$col_name_speed,
          "acc_lon_ms2",
          "pxx")

# dat_synth <-
#   dat_synth %>%
#   filter(passing %in% unique( get(sett_synth$df_name)$passing)[1:10] )



# Smooth acceleration values ----------------------------------------------

# ## Smooth acceleration values
# dat <- 
#   dat %>% 
#   group_by(passing) %>% 
#   mutate(acc_lon_ms2_smooth = smoothWithLoess(acc_lon_ms2, 1/10, 2, T))



# Visualize velocity profiles ---------------------------------------------

plot_speed_profiles <-
  ggplot() +
  geom_line(data = dat_synth,
            aes_string(x = sett_synth$col_name_am,
                       y = sett_synth$col_name_speed,
                       group = sett_synth$col_name_group),
            size = 1,
            colour = "grey85"
            ) +
  scale_x_continuous(expand = c(0, 0)) +
  coord_cartesian(ylim = c(0, 25), xlim = c(-80, 80)) +
  theme_bw()

#if (sett_synth$show_plot) plot(plot_speed_profiles)

# Estimate maximum acceleration after turning -----------------------------

dat_acc_lon_max <- 
  dat_synth %>%
  filter_(paste(sett_synth$col_name_am, ">=", sett_synth$am_limit)) %>%
  #filter_(paste(sett_synth$col_name_am, ">=", sett_synth$am_limit)) %>%
  group_by_(sett_synth$col_name_group) %>%
  mutate(speed_ms_am_limit = first(speed_ms)) %>% 
  mutate(acc_lon_ms2_max = max(acc_lon_ms2)) %>% 
  mutate(acc_lon_ms2_est_max = 
           acc_lon_ms2 / (
             -(speed_ms_am_limit / sett_idm$u_max)^sett_idm$delta + 1
           ) ) %>% 
  summarize(acc_lon_ms2_max = max(acc_lon_ms2_max),
            acc_lon_ms2_est_max = max(acc_lon_ms2_est_max)) %>%
  select(passing, acc_lon_ms2_max, acc_lon_ms2_est_max) %>% 
  data.frame()

## Merge data and maximum acceleration
dat_idm <- left_join(dat_synth, dat_acc_lon_max)


# Compute desired velocity profiles ---------------------------------------

dat_idm <- 
  dat_idm %>%
  ## As in formula 3 in Liebner et al. (2003)
  group_by(passing) %>% 
  mutate(speed_ms_u = 
           speed_ms / 
           ( 1 - acc_lon_ms2 /acc_lon_ms2_est_max )^(1 / sett_idm$delta) 
  ) %>% 
  mutate(speed_ms_u = na.locf(speed_ms_u, na.rm = F)) %>% 
  #rowwise() %>% mutate(speed_ms_u = min(speed_ms_u, sett_idm$u_max)) %>% 
  data.frame()

plot_speed_profiles + 
  geom_line(data = dat_idm,
            aes_string(x = sett_synth$col_name_am,
                y = "speed_ms_u",
                group = sett_synth$col_name_group))



# Smooth DVM curves -------------------------------------------------------

dat_idm <-
  dat_idm %>%
  group_by_(sett_synth$col_name_group) %>%
  #mutate(speed_ms_u_smoothed = smoothWithLoess(speed_ms_u, 1/20, 1,  onlyfitted = T)) %>%
  #mutate(speed_ms_u_smoothed = rollAvg(speed_ms_u, k = 8)) %>%
  mutate(speed_ms_u_smoothed = rollAvg(speed_ms_u, k = 25, align = "center")) %>%
  #mutate(speed_ms_u_smoothed = rollmean(speed_ms_u, k = 25, align = "center", fill = NA)) #%>%
  # mutate(speed_ms_u_smoothed = ifelse(pxx_dti_m_rnd1 >= sett_synth$am_limit,
  #                            sett_idm$u_max,
  #                            speed_ms_u_smoothed)) #%>% 
  group_by_(sett_synth$col_name_group, sett_synth$col_name_am) %>%
  mutate(speed_ms_u_smoothed = min(speed_ms_u_smoothed, sett_idm$u_max, na.rm = T))
  # mutate(speed_ms_u_smoothed = 
  #          ifelse(is.na(speed_ms_u_smoothed),
  #                 speed_ms_u,
  #                 speed_ms_u_smoothed))
ggplot() +
       geom_line(data = dat_idm,
                 aes(x = pxx_dti_m_rnd1,
                     y = speed_ms_u,
                     group = passing),
                 size = 1) +
       geom_line(data = dat_idm,
                 aes(x = pxx_dti_m_rnd1,
                     y = speed_ms_u_smoothed,
                     group = passing),
                 col = "red",
                 size = 0.75) +
  # scale_x_continuous(expand = c(0, 0)) +
  #      coord_cartesian(xlim = c(-50, 25),
  #                      ylim = c(0, 25)) +
       theme_bw()



# Limit DVM to maximum desired velocity  ----------------------------------

## Assuming u = umax (see Liebner et al., 2013)
# dat_idm <-
#   dat_idm %>%
#   group_by_(sett_synth$col_name_group,
#             sett_synth$col_name_am) %>%
#   mutate(speed_ms_u_limitted =
#            min(speed_ms_u, sett_idm$u_max, na.rm = T),
#          speed_ms_u_smoothed_limitted =
#            min(speed_ms_u_smoothed, sett_idm$u_max, na.rm = T))
# 





# Re-compute driven distance ----------------------------------------------

# dat_idm <- 
#   dat_idm %>%
#   #arrange_(sett_synth$col_name_group, sett_synth$col_name_time) %>% 
#   #mutate_(pxx_dist_m_by_u = paste(sett_synth$col_name_am)) %>% 
#   #mutate_(pxx_dist_m_by_u = -50) %>% 
#   group_by_(sett_synth$col_name_group) %>% 
#   mutate(pxx_dist_m_by_u = 
#            speed_ms_u_limitted * (time_s - lag(time_s))) %>% 
#   ## To avoid NA when computing cumsum
#   mutate(pxx_dist_m_by_u = 
#            ifelse(is.na(pxx_dist_m_by_u), 0 , pxx_dist_m_by_u)) %>% 
#   mutate(pxx_dist_m_by_u = cumsum(pxx_dist_m_by_u) - 55) %>% 
#   data.frame()



# Set speed to maximum u after distance limit -----------------------------

# dat_idm <- 
#   dat_idm %>% 
#   group_by_(sett_synth$col_name_group) %>% 
#   ## Add desired velocity for section after turning
#   mutate_(speed_ms_u_limitted =
#            paste("ifelse(", sett_synth$col_name_am, ">=", sett_synth$am_limit, ",",
#            #ifelse(pxx_dist_m_by_u >= sett_synth$am_limit,
#                   sett_idm$u_max, ",",
#                   "speed_ms_u_limitted", ")")) %>% 
#   mutate_(speed_ms_u_smoothed_limitted =
#             paste("ifelse(", sett_synth$col_name_am, ">=", sett_synth$am_limit, ",",
#                   #ifelse(pxx_dist_m_by_u >= sett_synth$am_limit,
#                   sett_idm$u_max, ",",
#                   "speed_ms_u_smoothed_limitted", ")")) 



# Visualize synthesized velocity curves -----------------------------------

plot_speed_profiles_synth <-
  plot_speed_profiles +
  #ggplot() + 
  geom_path(data = 
              dat_idm, #%>% 
              #filter(round_txt == "normal") %>% 
              #filter(subject_id %in% sett_synth$show_plot_by_subject),
            aes_string(
              x = sett_synth$col_name_am,
              #x = "pxx_dist_m_by_u",
              #y = "speed_ms_u",
              y = "speed_ms_u_smoothed",
              #y = "speed_ms_u_limitted",
              #y = "speed_ms_u_smoothed_limitted",
              group = sett_synth$col_name_group),
            size = 1,
            colour = "green3") +
   theme_bw()
 
if (sett_synth$show_plot) 
  plot(plot_speed_profiles_synth)
