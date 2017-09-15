
# Settings ----------------------------------------------------------------

sett_proc <- c()
sett_proc$df_name <- "study2_t_adtf_pxx_full_dist_m_rnd1_intrpld_cut"
sett_proc$pxx <- 2
sett_proc$colname_pxx <- "pxx"
sett_proc$colname_am <- "pxx_dist_m_rnd1"
sett_proc$colname_subject <- "subject_id"
sett_proc$colname_round <- "round_txt"
sett_proc$colname_group <- "passing"
sett_proc$colname_subject_round <- 
  paste_(sett_proc$colname_subject,
         sett_proc$colname_round)



# Prepare data ------------------------------------------------------------

dat_proc <- 
  get(sett_proc$df_name) %>% 
  filter(pxx %in% sett_proc$pxx) %>% 
  select(sett_proc$colname_subject,
         sett_proc$colname_round,
         sett_proc$colname_group,
         sett_proc$colname_pxx,
         sett_proc$colname_am,
         gps_lon,
         gps_lat) %>% 
  mutate_(.dots = 
            setNames(list(interp(~ paste(sprintf("s%02d", v), w, sep = "_"),
                                 v = as.name(sett_proc$colname_subject),
                                 w = as.name(sett_proc$colname_round))),
                     sett_proc$colname_subject_round))

## Get reference position
t_gps_reference_positions <- 
  dbGetSrc(db_conn_7, "t_gps_reference_positions") %>% 
  filter(position_id %in% sett_proc$pxx) %>% 
  select(position_id,
         gps_lon,
         gps_lat)



# Visualise GPS paths on map data -----------------------------------------

file_path <- file.path("resources/study-2/map-images")
file_list <- list.files(file_path)
file_list <- file_list[grepl("RData", file_list)]
file_list <- file_list[grepl("zoom19", file_list)]
filename <- file_list[grepl(sprintf("s%02d", sett_proc$pxx), file_list)]
load(file.path(file_path, filename))

plot_map_gps <- 
  ggmap(map) + 
  geom_path(data = dat_proc,
            aes(x = gps_lon,
                y = gps_lat,
                group = passing),
            colour = "white",
            alpha = "0.25") + 
  geom_point(data = t_gps_reference_positions,
             aes(x = gps_lon,
                 y = gps_lat),
             colour = "red",
             size = 2)

plot(plot_map_gps)



# Correct GPS latitude ----------------------------------------------------

## Get correction data
t_gps_correction_factor <- 
  dbGetSrc(sett_query$db_conn_name, 
           "t_gps_correction_factor")

## Merge original data and individual deviations (correction factors)
dat_proc_corr <- 
  left_join(dat_proc,
            dat_gps_dev %>% 
              select_(sett_proc$colname_subject_round, 
                      "gps_lon_dev",
                      "gps_lat_dev"),
            by = sett_proc$colname_subject_round) %>% 
  mutate(gps_lon_corr = gps_lon + gps_lon_dev,
         gps_lat_corr = gps_lat + gps_lat_dev)

## Correction only necessary for latitude !!!


# Visualise corrected values ----------------------------------------------

plot_map_gps_corr <- 
  ggmap(map) + 
  geom_path(data = dat_proc_corr,
            aes_string(x = "gps_lon",
                       y = "gps_lat_corr",
                       group = sett_proc$colname_group),
            colour = "white",
            alpha = "0.25") +
  geom_point(data = t_gps_reference_positions,
             aes(x = gps_lon,
                 y = gps_lat),
             colour = "red",
             size = 2) 

plot(plot_map_gps_corr)
