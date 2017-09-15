
# Fetch data --------------------------------------------------------------

## subject_id = 1 as first subject without 
dat_gps <- 
  dbGetQuery(db_conn_6, 
             "SELECT * 
             FROM t_adtf_p01_gps_dist 
             WHERE subject_id = 3 AND round_id = 1")

dat_pos <- 
  dbGetSrc(db_conn_6, 
           "t_gps_reference_positions")



# Add round helper information --------------------------------------------

## rh1 = round_helper1
## rh2 = round_helper2

dat_gps <- 
  dat_gps %>% 
  mutate(rh1 = 1,
         rh2 = 1) %>% 
  mutate(rh1 = ifelse(dist_m < 4000, 1, rh1),
         rh2 = ifelse(dist_m < 4000, 2, rh2)) %>% 
  mutate(rh1 = ifelse(dist_m >= 4000 & dist_m < 5000, 1, rh1),
         rh2 = ifelse(dist_m >= 4000 & dist_m < 5000, 1, rh2)) %>% 
  mutate(rh1 = ifelse(dist_m >= 5000 & dist_m <= 6000, 2, rh1),
         rh2 = ifelse(dist_m >= 5000 & dist_m <= 6000, 1, rh2)) %>% 
  mutate(rh1 = ifelse(dist_m > 6000, 2, rh1),
         rh2 = ifelse(dist_m > 6000, 2, rh2))
  



# Visualise track, round helper, and position numbers ---------------------

## Remove first row with gps latitude and gps longitude being zerp
#dat_gps <- dat_gps[2:nrow(dat_gps), ]

plot_track_positions_rh1 <- 
  ggplot() + 
  geom_path(data = dat_gps,
            aes(x = gps_lon,
                y = gps_lat,
                colour = as.factor(rh1)),
            size = 2,
            alpha = 0.5) + 
  geom_text(data = dat_pos,
            aes(x = gps_lon,
                y = gps_lat,
                label = position_id)) + 
  scale_colour_manual(values = c("red3", "green3"),
                     labels = c("round_helper1 = 1",
                                "round_helper1 = 2")) + 
  theme_bw() 

plot_track_positions_rh2 <- 
  ggplot() + 
  geom_path(data = dat_gps,
            aes(x = gps_lon,
                y = gps_lat,
                colour = as.factor(rh2)),
            size = 2,
            alpha = 0.5) + 
  geom_text(data = dat_pos,
            aes(x = gps_lon,
                y = gps_lat,
                label = position_id)) +
  scale_color_manual(values = c("red3", "green3"),
                     labels = c("round_helper2 = 1",
                                "round_helper2 = 2")) + 
  theme_bw() 

grid.arrange(plot_track_positions_rh1, 
             plot_track_positions_rh2)
