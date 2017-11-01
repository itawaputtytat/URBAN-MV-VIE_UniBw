
## Find positions where steer_angle_deg is max
## Classify passings as outliers when position lies outsie the 99.5 quantile

# Preparatory settings ----------------------------------------------------

sett_expl <- c()
sett_expl$df_name$data <- paste_(sett_query$df_name, "intrpld")
sett_expl$df_name$intersection_attributes <- "t_pxx_intersection_attributes"
#sett_expl$pxx <- sett_query$pxx
sett_expl$pxx <- 18
sett_expl$col_name_am <- sett_query$col_name_am
sett_expl$col_name_case <- "passing"
sett_expl$col_name_group <- "round_txt"
sett_expl$col_name_sa <- "steer_angle_deg"
sett_expl$am_thresholds_sa_max <- c(-20, 25)
sett_expl$am_thresholds_sa_min1 <- c(-50, 50)
sett_expl$am_thresholds_sa_min2 <- c(-50, 50)
sett_expl$sa_threshold_min <- 80
sett_expl$z_cut_off <- 1.96



# Prepare data ------------------------------------------------------------

dat <- 
  get(sett_expl$df_name$data) %>% 
  filter(pxx %in% sett_expl$pxx) %>% 
  select_(sett_expl$col_name_group,
          sett_expl$col_name_case,
          sett_expl$col_name_am,
          sett_expl$col_name_sa)


test <- identifySteerAngleOutlier(dat,
                                  sett_expl$col_name_am,
                                  sett_expl$col_name_case,
                                  sett_expl$col_name_group,
                                  sett_expl$col_name_sa,
                                  sett_expl$am_thresholds_sa_max,
                                  sett_expl$am_thresholds_sa_min1,
                                  sett_expl$am_thresholds_sa_min2,
                                  sett_expl$sa_threshold_min,
                                  sett_expl$z_cut_off)



# Visualize original SA values --------------------------------------------

plot_steer <-
  ggplot() +
  geom_line(data = dat,
            aes_string(x = sett_expl$col_name_am,
                       y = sett_expl$col_name_sa,
                       group = sett_expl$col_name_case,
                       colour = sett_expl$col_name_group)) +
  coord_cartesian(xlim = c(min(dat[, sett_expl$col_name_am]),
                           max(dat[, sett_expl$col_name_am])),
                  ylim = c(-600, 600)) + 
  guides(colour = F) + 
  facet_grid(.~round_txt) + 
  ggtitle("Original values",
          subtitle = paste("Intersection:", sett_expl$pxx))

#plot(plot_steer)



# Visualize AM of max. SA -------------------------------------------------

plot_steer_max <- 
  plot_steer + 
  geom_vline(data = test$dat_max_summary,
             aes_string(xintercept = 
                          paste_(sett_expl$col_name_am, "min_median"),
                        color = "round_txt")) + 
  geom_vline(xintercept = test$dat_am_steer_max)

#plot(plot_steer_max)


# Visualize AM of min. SA before turning ----------------------------------

plot_steer_min1 <- 
  plot_steer_max + 
  geom_vline(data = test$dat_min1_summary,
             aes_string(xintercept = 
                          paste_(sett_expl$col_name_am, "max_median"),
                        color = "round_txt")) + 
  geom_vline(xintercept = test$dat_am_steer_min1)

#plot(plot_steer_min1)


# Visualize AM of min. SA after turning -----------------------------------

plot_steer_min2 <- 
  plot_steer_min1 + 
  geom_vline(data = dat_min2_summary,
             aes_string(xintercept = 
                          paste_(sett_expl$col_name_am, "min_median"),
                        color = "round_txt")) + 
  geom_vline(xintercept = test$dat_am_steer_min2)

#plot(plot_steer_min2)



# Visualize outlier -------------------------------------------------------

plot_outlier <- 
  #ggplot() + 
  plot_steer_min2 +
  geom_line(data = dat,
            aes_string(x = sett_expl$col_name_am,
                       y = sett_expl$col_name_sa,
                       group = sett_expl$col_name_case)) +
  geom_line(data = test$dat_outlier %>% filter(is_outlier),
            aes_string(x = sett_expl$col_name_am,
                       y = sett_expl$col_name_sa,
                       group = sett_expl$col_name_case),
            size = 1,
            color = "red")


plot(plot_outlier)



# 
# outlier <- dat_max %>% filter(is_outlier) %>% pull(passing)
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
