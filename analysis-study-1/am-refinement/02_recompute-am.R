
# Interpolate data --------------------------------------------------------

intrpldf_batch4rb(dat_study1_t_adtf_pxx_full, 
                  #col_name_ref = sett_query$col_name_am, 
                  col_name_ref = "dti_m", 
                  stepsize = 0.1,
                  binary_vars = "brake_status",
                  suffix = "intrpld", 
                  outputFlag = T)



# Get reference position --------------------------------------------------

t_gps_reference_positions <- 
  dbGetSrc(dbFindConnObj("Study-1"),
           "t_gps_reference_positions") %>% 
  filter(position_id == 6)



# Get round information ---------------------------------------------------

t_experimental_conditions <- 
  dbGetSrc(dbFindConnObj("Study-1"),
           "t_experimental_conditions") %>% 
  filter(!subject_id %in% c(19))



# Re-compute GPS distance -------------------------------------------------

dat_study1_t_adtf_pxx_full_intrpld <- 
  dat_study1_t_adtf_pxx_full_intrpld %>% 
  rowwise() %>% 
  mutate(gps_dist_m = 
           distm(c(gps_lon, 
                   gps_lat), 
                 c(t_gps_reference_positions$gps_lon, 
                   t_gps_reference_positions$gps_lat), 
                 fun = distVincentySphere))



# Re-compute distance to intersection -------------------------------------

dat_study1_t_adtf_pxx_full_intrpld <- 
  dat_study1_t_adtf_pxx_full_intrpld %>% 
  ungroup() %>% 
  group_by(passing) %>% 
  mutate(driven_distance_m_min = 
           ifelse(gps_dist_m == min(gps_dist_m),
                  driven_distance_m,
                  NA)) %>% 
  mutate(driven_distance_m_min = max(driven_distance_m_min, na.rm = T)) %>% 
  mutate(dti_m_v2 = driven_distance_m_min - driven_distance_m)



# Summarize distances -----------------------------------------------------

t_adtf_p04_gps_distance_min <- 
  dbGetSrc(dbFindConnObj("Study-1"),
           "t_adtf_p04_gps_distance_min")

dists <- 
  dat_study1_t_adtf_pxx_full_intrpld %>% 
  ungroup() %>% 
  group_by(subject_id, round_id) %>% 
  summarize(gps_dist_m_min_v2 = min(gps_dist_m)) %>% 
  left_join(t_experimental_conditions) %>% 
  mutate(round_nr = 
           ifelse(round_id == "normal",
                  round_nr_normal, 
                  round_nr_stress)) %>% 
  mutate(round_nr = 
           ifelse(round_id == "intro",
                  1, round_nr)) %>% 
  left_join(t_adtf_p04_gps_distance_min) %>% 
  mutate(gps_dist_m_diff = gps_distance_m_min - gps_dist_m_min_v2) %>% 
  mutate(gps_dist_m_diff = round(gps_dist_m_diff, 5)) %>% 
  select(subject_id, round_nr, round_id, gps_distance_m_min, gps_dist_m_min_v2, gps_dist_m_diff) %>%
  #arrange(subject_id, round_nr) %>% 
  data.frame()



# Viz ---------------------------------------------------------------------

sid <- 30
rid <- "stress"

test <- 
  dat_study1_t_adtf_pxx_full %>% 
  filter(subject_id == sid & round_id == rid) %>% 
  filter(dti_m == 0)

test_intrpl <- 
  dat_study1_t_adtf_pxx_full_intrpld %>% 
  filter(subject_id == sid & round_id == rid) %>% 
  filter(dti_m_v2 == 0)

ggplot() + 
  geom_point(data = dat_study1_t_adtf_pxx_full %>% 
               filter(subject_id == sid & round_id == rid) %>% 
               filter(dti_m >= -20 & dti_m <= 20),
             aes(x = gps_lon,
                 y = gps_lat),
             size = 1,
             col = "red") + 
  geom_point(data = t_gps_reference_positions %>% 
               filter(position_id == 6),
             aes(x = gps_lon,
                 y = gps_lat),
             color = "red",
             size = 3) + 
  geom_point(data = dat_study1_t_adtf_pxx_full_intrpld %>% 
               filter(intrpld) %>% 
               filter(subject_id == sid & round_id == rid) %>% 
               filter(dti_m_v2 >= -20 & dti_m_v2 <= 20),
             aes(x = gps_lon,
                 y = gps_lat),
             size = 0.5,
             alpha = 0.5) + 

  geom_point(data = test_intrpl,
             aes(x = gps_lon,
                 y = gps_lat),
             color = "green",
             size = 2) + 
  geom_point(data = test,
             aes(x = gps_lon,
                 y = gps_lat),
             color = "blue",
             size = 2)
