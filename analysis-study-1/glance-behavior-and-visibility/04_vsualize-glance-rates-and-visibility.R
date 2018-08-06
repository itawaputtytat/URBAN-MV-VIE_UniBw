
## Requires glance rate computation

# Preparatory settings ----------------------------------------------------

sett_dat <- c()
sett_dat$df_name <- "dat_study1_t_glances_pxx_full_dti_rnd1_intrpld_cut"
sett_dat$col_name_act_level <- "glance_dir_level_v2"
sett_dat$col_name_act_label <- "glance_dir_label_v2"
sett_dat$col_name_case <- "passing"
sett_dat$col_name_am <- "pxx_dti_m_rnd1"

sett_proc < c()
sett_proc$position <- 5

sett_act <- c()
sett_act$level <- c(0, 1, 2, 3, 4, 6)

sett_plot <- c()
sett_plot$glances$color$undefined <- "black"
sett_plot$glances$color$ahead <- "blue4"
sett_plot$glances$color$other <- "darkgray"
sett_plot$glances$color$rear <- "orange"
sett_plot$glances$color$left <- "red1"
sett_plot$glances$color$right <- "green4"



# Prepare data ------------------------------------------------------------

dat <- 
  get(sett_dat$df_name) %>% 
  filter_(paste(sett_dat$col_name_am, "<= 0")) %>% 
  filter(pxx == sett_proc$position) %>% 
  filter(round_txt == "normal")

dat2 <- dat
dat2[, sett_dat$col_name_act_level] <- 
  factor(dat2[, sett_dat$col_name_act_level],
         #levels = sort(unique(dat2[, sett_dat$col_name_act_level])),
         levels = sett_act$level,
         labels = names(sett_plot$glances$color))

dat2[, sett_dat$col_name_case] <- 
  factor(dat2[, sett_dat$col_name_case])

dat2[, "is_stopping_dist1_m50_dist2_0_speed_5"] <- 
  factor(dat2[, "is_stopping_dist1_m50_dist2_0_speed_5"])



# Load visibility data ----------------------------------------------------

## Load visibility data (raw calculations)
dat_visibility_perc <- 
  dbGetSrc(dbFindConnObj("Study-1"), 
           "t_visibility_perc")

## Filter for selected position
dat_visibility_perc <- 
  dat_visibility_perc %>% 
  filter_(paste0("position_id", "==", sett_proc$position))

## Load visibility data with smoothed values
dat_visibility_perc_smoothed <- 
  dbGetSrc(dbFindConnObj("Study-1"), 
           "t_visibility_perc_smoothed")

## Filter for selected position
dat_visibility_perc_smoothed <- 
  dat_visibility_perc_smoothed %>% 
  filter_(paste0("position_id", "==", sett_proc$position))



# Visualisation of visibilty curve and area -------------------------------

plot_visibility <-
  ggplot() +
  # ## Area
  # geom_ribbon(data = dat_visibility_perc_smoothed,
  #             aes_string(x = sett_dat$col_name_am,
  #                        ymax = "width_intersection_center_to_left_perc",
  #                        ymin = "left_ymin"),
  #             fill = "red3",
  #             alpha = 0.2,
  #             position = "identity") +
  # geom_ribbon(data = dat_visibility_perc_smoothed,
  #             aes_string(x = sett_dat$col_name_am,
  #                        ymax = "width_intersection_center_to_right_perc",
  #                        ymin = "right_ymin"),
  #             fill = "green4",
  #             alpha = 0.2,
  #             position = "identity") 
  # geom_ribbon(data = dat_visibility_perc_smoothed,
  #           aes_string(x = sett_dat$col_name_am,
  #                      ymax = "width_intersection_center_to_left_perc",
  #                      ymin = "width_intersection_center_to_right_perc"),
  #                      fill = "red3",
  #                      alpha = 0.2,
  #           position = "identity") +


  ## OLD GOOD STUFF
  geom_area(data = dat_visibility_perc_smoothed,
          aes_string(x = sett_dat$col_name_am,
                     y = "width_intersection_center_to_left_perc",
                     fill = shQuote("left")),
          alpha = 0.5,
          position = "identity") +
  geom_area(data = dat_visibility_perc_smoothed,
            aes_string(x = sett_dat$col_name_am,
                       y = "width_intersection_center_to_right_perc",
                       fill = shQuote("right")),
            alpha = 0.5,
            position = "identity") +
  geom_line(data = dat_visibility_perc_smoothed,
            aes_string(x = sett_dat$col_name_am,
                       y = "width_intersection_center_to_left_perc"),
            color = "red3",
            alpha = 0.7,
            size = 0.1) +
  geom_line(data = dat_visibility_perc_smoothed,
            aes_string(x = sett_dat$col_name_am,
                       y = "width_intersection_center_to_right_perc"),
            color = "green4",
            alpha = 0.7,
            size = 0.1) +
  geom_line(data = glances_rates1st,
            aes_string(x = sett_dat$col_name_am,
                       y = "act_nr_PER_act_level_1st_ON_cases_n__4",
                       color = shQuote("left")),
            size = 0.5) +
  geom_line(data = glances_rates1st,
            aes_string(x = sett_dat$col_name_am,
                       y = "act_nr_PER_act_level_1st_ON_cases_n__6",
                       color = shQuote("right")),
            size = 0.5) 

plot_visibility <-
  plot_visibility + 
  scale_color_manual(
    name   = "Direction",
    values = c("red3", "green4"),
    labels = c("left", "right")) + 
  scale_fill_manual(
    name   = "Direction",
    values = c("#f5cccc", "#cce8cc"),
    labels = c("left", "right")) 
  


## Add descriptive text elements
plot_visibility <- 
  plot_visibility +
  labs(title = "Evolution of first glances",
       subtitle = "Compared to intersection visibility",
       x = "Distance to intersection",
       y = "Percentage") + 
  scale_x_continuous(expand = c(0, 0)) +
  coord_cartesian(xlim = c(-50, 0)) 



# Visualisation of visibilty curve and area (Postproduction) --------------

plot_visibility <- 
  plot_visibility +
  theme_thesis() +
  ## Top right inside panel
  theme(legend.position = c(0.0025, 0.9955),
        legend.justification = ggplot2_legendJustification("left", "top")) +
  theme(legend.margin=margin(0.1, 0.1, 0.1, 0.1, "cm")) +
  # ## Top left above panel
  # theme(legend.margin=margin(0, 0.1, 0, 0.1, "cm")) +
  # theme(legend.box.margin=margin(0, 0, -1.4, 0, "cm")) +
  # ## Top right next to panel
  # theme(legend.margin=margin(0.05, 0.05, 0.05, 0.05, "cm")) +
  # theme(legend.box.margin=margin(0, -0.1, 0, -0.3, "cm")) +
  # theme(legend.justification = "top") +
  #theme(legend.position="top") +
  # #theme(legend.justification = -0.0075) +
  # #theme(legend.justification = -0.06) +
  # theme(legend.justification = 0) +
  # #theme(legend.margin=margin(t = 0, r = 0, b = -0.25, l = 0, unit="cm")) +
  theme(legend.key.size = unit(0.65, "line"),
        legend.key = element_blank())

plot(plot_visibility)


ggsave("test.png",
       plot_visibility,
       dpi = 600,
       width = 8,
       height = 6,
       units = "cm")
        
        


# fsdfsdf -----------------------------------------------------------------









  ## Curve (KEEP TO UNDERSTAND DIRECTION AS FILL AND COLOUR GUIDE)
  # geom_line(data = sxx_visibility.2plot.tidy,
  #           aes_string(x = set4plot$distvar,
  #                      y = "percentage",
  #                      colour = "direction"),
  #           size = 0.5) +
  
  ## In case: Coordinate in background
  # geom_hline(yintercept=seq(-12.5, 100, 25), colour = "grey98", size = 0.5) +
  # geom_hline(yintercept=seq(0, 100, 25), colour = "grey90", size = 0.2) +
  # geom_vline(xintercept=seq(-50, 50, 10), colour = "grey98", size = 0.5) +
  # geom_vline(xintercept=seq(-55, 50, 5), colour = "grey90", size = 0.2) +
  
  ## ... and then: Areas in foreground
  # geom_area(data = sxx_visibility.2proc,
  #           aes(x = sxx_dist_m_rnd1,
  #               y = width_left2roadcenter_rel),
#               #size = 2,
#               fill = "red4",
#           alpha = 0.25) +
# geom_area(data = sxx_visibility.2proc,
#           aes(x = sxx_dist_m_rnd1,
#               y = width_right2roadcenter_rel),
#           #size = 2,
#           fill = "green4",
#           alpha = 0.25) +

## Adjust aestehtics
# scale_alpha_manual(values = c(0.15, 0.15)) +
#   scale_colour_manual(values = c("red4", "green4")) +
#   #  scale_fill_manual(values = c("#eed9d9", "#d9eed9")) +
#   scale_fill_manual(values = c("red2", "green2")) +
#   
#   coord_cartesian(xlim = c(min(dat2plot_measured[, set4plot$distvar]),
#                            max(dat2plot_measured[, set4plot$distvar])),
#                   ylim = c(0, 105)) +
#   scale_x_continuous(expand = c(0,0)) +
#   scale_y_continuous(expand = c(0,0))
