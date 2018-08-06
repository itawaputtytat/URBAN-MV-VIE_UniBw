
## Compute visibilty (percentage) to each side based on xy-coordinates
## ... found in videos

# Preparatory settings ----------------------------------------------------

sett_dat <- c()
sett_dat$db_conn_name <- dbFindConnObj("Study-1")
sett_dat$src_name <- "t_visibility_annotations"

sett_proc <- c()
sett_proc$img_width <- 800
sett_proc$img_height <- 600
sett_proc$img_center <- sett_proc$img_width / 2



# Data loading and preprocessing ------------------------------------------

dat_visibility <- dbGetSrc(sett_dat$db_conn_name, sett_dat$src_name)

## Sort data
dat_visibility <-
  dat_visibility %>%
  group_by(position_id, image_nr) %>%
  arrange(position_id, image_nr)



# Compute visibility width and percentage ---------------------------------

## Reference: Image center
dat_visibility <-
  dat_visibility %>%
  ## Compute width to image center
  mutate(width_img_center_to_left = 
           sett_proc$img_center - pos_x,
         width_img_center_to_right = 
           pos_x + width - sett_proc$img_center) %>% 
  ## Compute percentage to image center
  mutate(width_img_center_to_left_perc = 
           100 / sett_proc$img_center * width_img_center_to_left,
         width_img_center_to_right_perc = 
           100 / sett_proc$img_center * width_img_center_to_right)


## Reference: Road center
dat_visibility <-
  dat_visibility %>%
  ## Compute width to road center
  mutate(width_intersection_center_to_left = 
           pos_x_intersection_center - pos_x,
         width_intersection_center_to_right = 
           pos_x + width - pos_x_intersection_center) %>% 
  ## Compute percentage to road center
  mutate(width_intersection_center_to_left_perc = 
           100 / pos_x_intersection_center * width_intersection_center_to_left,
         width_intersection_center_to_right_perc = 
           100 / (800 - pos_x_intersection_center) * width_intersection_center_to_right)
  

  
# Write to database -------------------------------------------------------

dbWriteTable(get(sett_dat$db_conn_name), 
             "t_visibility_percentage", 
             dat_visibility,
             row.names = F,
             overwrite = T)
