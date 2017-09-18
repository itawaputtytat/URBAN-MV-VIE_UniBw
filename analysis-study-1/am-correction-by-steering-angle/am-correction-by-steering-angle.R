
## Find positions where steer_angle_deg is max
## Classify passings as outliers when position lies outsie the 99.5 quantile

# Preparatory settings ----------------------------------------------------

sett_expl <- c()
sett_expl$df_name$data <- paste_(sett_query$df_name, "intrpld")
sett_expl$df_name$intersection_attributes <- "t_pxx_intersection_attributes"
#sett_expl$pxx <- sett_query$pxx
sett_expl$pxx <- 12
sett_expl$col_name_am <- sett_query$col_name_am
sett_expl$col_name_group <- "passing"
sett_expl$col_name_round <- "round_txt"
sett_expl$col_name_steer_angle <- "steer_angle_deg"
sett_expl$col_name_speed <- "speed_ms"
sett_expl$col_name_speed_max <- paste_(sett_expl$col_name_speed, "max")
sett_expl$dist_max <- 18 ## Set threshold for distance for finding maximum steering angle
sett_expl$dti_min <- -15 ## Special case for situation 3 see below! (next section)
sett_expl$treshold_factor <- 1
sett_expl$plot <- F



# Prepare data ------------------------------------------------------------

dat_steer <- 
  get(sett_expl$df_name$data) %>% 
  filter(pxx == sett_expl$pxx) %>% 
  select_(sett_expl$col_name_round,
          sett_expl$col_name_group,
          sett_expl$col_name_am,
          sett_expl$col_name_steer_angle)



# Visualize original SA values --------------------------------------------

plot_steer <-
  ggplot() +
  geom_line(data = dat_steer,
            aes_string(x = sett_expl$col_name_am,
                       y = sett_expl$col_name_steer_angle,
                       group = sett_expl$col_name_group,
                       colour = sett_expl$col_name_round)) +
  coord_cartesian(xlim = c(min(dat_steer[, sett_expl$col_name_am]),
                           max(dat_steer[, sett_expl$col_name_am])),
                  ylim = c(-600, 600)) + 
  guides(colour = F) + 
  facet_grid(.~round_txt) + 
  ggtitle("Original values",
          subtitle = paste("Intersection:", sett_expl$pxx))

#plot(plot_steer)



# Identify AM of max. SA --------------------------------------------------

## Find position where steer_angle_deg == max(abs(steer_angle_deg))
dat_steer_max <- 
  dat_steer %>%
  filter_(paste(sett_expl$col_name_am, "<=", 25, "&",
                sett_expl$col_name_am, ">=", -20)) %>% 
  group_by_(sett_expl$col_name_round, 
            sett_expl$col_name_group) %>%
  filter_( paste("abs(", sett_expl$col_name_steer_angle, ")", "==", 
                 "max(abs(", sett_expl$col_name_steer_angle, "))") ) %>% 
  ## Summarize values including AM values
  ## (in case of duplicates with same value take the minimum position)
  ## ... and also the corresponding speed
  summarise_(.dots = c(
    setNames(list(interp(~ min(v), v = as.name(sett_expl$col_name_am))),
             paste_(sett_expl$col_name_am, "min")),
    setNames(list(interp(~ max(v), v = as.name(sett_expl$col_name_steer_angle))),
             paste_(sett_expl$col_name_steer_angle, "max"))
  )) %>% 
  arrange_(paste_(sett_expl$col_name_am, "min"))



# Summarize data corresponding to max. SA ---------------------------------

dat_steer_max_summary <-
  computeSummary(dat_steer_max,
                 c("round_txt"),
                 c(paste_(sett_expl$col_name_am, "min"),
                   paste_(sett_expl$col_name_steer_angle, "max")),
                 c("min", "max", "mean", "sd", "median"))




# Extract AM of max. SA ---------------------------------------------------

dat_am_steer_max <- 
  dat_steer_max_summary %>% 
  select(pxx_dti_m_rnd1_min_median) %>%
  pull() %>% 
  as.vector() %>% 
  median()



# Visualize AM of max. SA -------------------------------------------------

plot_steer_max <- 
  plot_steer + 
  geom_vline(data = dat_steer_max_summary,
             aes_string(xintercept = 
                          paste_(sett_expl$col_name_am, "min_median"),
                        color = "round_txt")) + 
  geom_vline(xintercept = dat_am_steer_max)

#plot(plot_steer_max)



# Identify AM of min. SA before turning -----------------------------------

dat_steer_min1 <- 
  #dat_steer %>% 
  left_join(dat_steer,
            dat_steer_max %>%
              select(passing,
                     sa_max_dti = pxx_dti_m_rnd1_min)) %>%
  filter_(paste(sett_expl$col_name_am, ">=", -50, "&",
          sett_expl$col_name_am, "<=", 50)) %>% 
  filter_(paste(sett_expl$col_name_steer_angle, "<=", 80)) %>% 
  filter_(paste(sett_expl$col_name_am, "<=", dat_am_steer_max)) %>% 
  #filter_(paste(sett_expl$col_name_am, "<= sa_max_dti")) %>% 
  group_by_(sett_expl$col_name_round, 
            sett_expl$col_name_group) %>%
  # filter_( paste("abs(", sett_expl$col_name_steer_angle, ")", "==", 
  #                "min(abs(", sett_expl$col_name_steer_angle, "))") ) %>% 
  ## Summarize values including AM values
  ## (in case of duplicates with same value take the minimum position)
  ## ... and also the corresponding speed
  summarise_(.dots = c(
    setNames(list(interp(~ max(v), v = as.name(sett_expl$col_name_am))),
             paste_(sett_expl$col_name_am, "max")),
    setNames(list(interp(~ max(v), v = as.name(sett_expl$col_name_steer_angle))),
             paste_(sett_expl$col_name_steer_angle, "min"))
  )) %>% 
  arrange_(paste_(sett_expl$col_name_am, "max"))



# Summarize data corresponding to min. SA before turning ------------------

dat_steer_min1_summary <-
  computeSummary(dat_steer_min1,
                 c("round_txt"),
                 c(paste_(sett_expl$col_name_am, "max"),
                   paste_(sett_expl$col_name_steer_angle, "min")),
                 c("min", "max", "mean", "sd", "median"))



# Extract AM of min. SA before turning ------------------------------------

dat_am_steer_min1 <- 
  dat_steer_min1_summary %>% 
  select_(paste_(sett_expl$col_name_am, "max_median")) %>%
  pull() %>% 
  as.vector() %>% 
  median()



# Visualize AM of min. SA before turning ----------------------------------

plot_steer_min1 <- 
  plot_steer_max + 
  geom_vline(data = dat_steer_min1_summary,
             aes_string(xintercept = 
                          paste_(sett_expl$col_name_am, "max_median"),
                        color = "round_txt")) + 
  geom_vline(xintercept = dat_am_steer_min1)

#plot(plot_steer_min1)



# Identify AM of min. SA after turning ------------------------------------

dat_steer_min2 <- 
  dat_steer %>%
  filter_(paste(sett_expl$col_name_am, ">=", -50, "&",
                sett_expl$col_name_am, "<=", 50)) %>% 
  filter_(paste(sett_expl$col_name_am, ">=", dat_am_steer_max)) %>% 
  filter_(paste(sett_expl$col_name_steer_angle, "<=", 80)) %>% 
  group_by_(sett_expl$col_name_round, 
            sett_expl$col_name_group) %>%
  # filter_( paste("abs(", sett_expl$col_name_steer_angle, ")", "==", 
  #                "min(abs(", sett_expl$col_name_steer_angle, "))") ) %>% 
  ## Summarize values including AM values
  ## (in case of duplicates with same value take the minimum position)
  ## ... and also the corresponding speed
  summarise_(.dots = c(
    setNames(list(interp(~ min(v), v = as.name(sett_expl$col_name_am))),
             paste_(sett_expl$col_name_am, "min")),
    setNames(list(interp(~ max(v), v = as.name(sett_expl$col_name_steer_angle))),
             paste_(sett_expl$col_name_steer_angle, "min"))
  )) %>% 
  arrange_(paste_(sett_expl$col_name_am, "min"))



# Summarize data corresponding to min. steering angle after turning -------

dat_steer_min2_summary <-
  computeSummary(dat_steer_min2,
                 c("round_txt"),
                 c(paste_(sett_expl$col_name_am, "min"),
                   paste_(sett_expl$col_name_steer_angle, "min")),
                 c("min", "max", "mean", "sd", "median"))



# Extract AM of min. SA after turning -------------------------------------

dat_am_steer_min2 <- 
  dat_steer_min2_summary %>% 
  select_(paste_(sett_expl$col_name_am, "min_median")) %>%
  pull() %>% 
  as.vector() %>% 
  median()



# Visualize AM of min. SA after turning -----------------------------------

plot_steer_min2 <- 
  plot_steer_min1 + 
  geom_vline(data = dat_steer_min2_summary,
             aes_string(xintercept = 
                          paste_(sett_expl$col_name_am, "min_median"),
                        color = "round_txt")) + 
  geom_vline(xintercept = dat_am_steer_min2)

#plot(plot_steer_min2)



# Identify outliers -------------------------------------------------------

dat_steer_max_outlier <- 
  dat_steer_max %>% 
  mutate(diff = pxx_dti_m_rnd1_min - dat_am_steer_max) %>% 
  mutate(is_outlier = codeOutliersZ(diff, zCutOff = 1.96)) %>% 
  #mutate(outlier = codeOutliersZ(diff, zCutOff = 1.96)) %>% 
  select(passing, outlier_steer_max = is_outlier)

dat_steer_min1_outlier <- 
  dat_steer_min1 %>% 
  mutate(diff = pxx_dti_m_rnd1_max - dat_am_steer_min1) %>% 
  mutate(is_outlier = codeOutliersZ(diff, zCutOff = 1.96)) %>% 
  #mutate(outlier = codeOutliersZ(diff, zCutOff = 1.96)) %>% 
  select(passing, outlier_steer_min1 = is_outlier)

dat_steer_min2_outlier <- 
  dat_steer_min2 %>% 
  mutate(diff = pxx_dti_m_rnd1_min - dat_am_steer_min2) %>% 
  mutate(is_outlier = codeOutliersZ(diff, zCutOff = 1.96)) %>% 
  #mutate(outlier = codeOutliersZ(diff, zCutOff = 1.96)) %>% 
  select(passing, outlier_steer_min2 = is_outlier)

dat_outlier <- 
  left_join(dat_steer_max_outlier,
            left_join(dat_steer_min1_outlier,
                      dat_steer_min2_outlier)) %>% 
  group_by(passing) %>% 
  mutate(outlier_sum = sum(outlier_steer_max,
                           outlier_steer_min1,
                           outlier_steer_min2)) %>% 
  mutate(is_outlier = ifelse(outlier_sum == 3, T, F)) %>% 
  arrange(outlier_sum) %>% 
  data.frame()

print(dat_outlier %>% filter(is_outlier))

# dat_steer_max <- 
#   dat_steer_max %>% 
#   mutate(diff = pxx_dti_m_rnd1_min - pos4deg_max) %>% 
#   mutate(is_outlier = codeOutliersZ(diff, zCutOff = 2.58)) %>% 
#   #mutate(outlier = codeOutliersZ(diff, zCutOff = 1.96)) %>% 
#   data.frame()
# 
dat_steer_outlier <-
  left_join(dat_steer,
            dat_outlier %>% select(passing, is_outlier))



# Visualize outlier -------------------------------------------------------

plot_outlier <- 
  #ggplot() + 
  plot_steer_min2 +
  geom_line(data = dat_steer,
            aes_string(x = sett_expl$col_name_am,
                       y = sett_expl$col_name_steer_angle,
                       group = sett_expl$col_name_group)) +
  geom_line(data = dat_steer_outlier %>% filter(is_outlier),
            aes_string(x = sett_expl$col_name_am,
                       y = sett_expl$col_name_steer_angle,
                       group = sett_expl$col_name_group),
            size = 1,
            color = "red")

if (sett_expl$plot) {
  #graphics.off()
  #dev.new()
  plot(plot_outlier)
}



# 
# outlier <- dat_steer_max %>% filter(is_outlier) %>% pull(passing)
# print(outlier)
# 
# plot_gps_path <- plotGPSPath(test_merge, sett_expl$pxx, c(outlier))
# 
# plot_gps_path_max_steer <- 
#   plot_gps_path + 
#   geom_point(data = 
#                test_merge %>% filter(
#                    pxx_dti_m_rnd1 == pxx_dti_m_rnd1_min),
#              aes(x = gps_lon, 
#                  y = gps_lat),
#              colour = "white",
#              alpha = 0.25,
#              size = 2) + 
#   geom_point(data = 
#                test_merge %>% 
#                filter(
#                  passing %in% outlier &
#                         pxx_dti_m_rnd1 == pxx_dti_m_rnd1_min),
#              aes(x = gps_lon, 
#                  y = gps_lat),
#              colour = "blue",
#              size = 2)
# 
# plot(plot_gps_path_max_steer)

# map <- getMapImage(sett_expl$pxx, zoom = 19)
# 
# ggmap(map) + 
#   geom_path(dat = test_merge %>% filter(passing == "p14_stress_s22"),
#             aes(x = gps_lon,
#                 y = gps_lat),
#             color = "yellow")
