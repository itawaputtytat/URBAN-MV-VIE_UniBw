ggplot() + 
  geom_line(data = can_v1_sxx_dist_m_rnd1_rb %>% 
              filter(sxx == 1),
            aes(x = sxx_dist_m_rnd1,
                y = speed_kmh,
                group = subid),
            size = 3) + 
  facet_grid(.~round_txt) + 
  geom_line(data = adtf_v2_sxx_dist_m_rnd1_rb %>% 
              filter(sxx == 1),
            aes(x = sxx_dist_m_rnd1,
                y = speed_kmh,
                group = subject_id),
            colour = "green",
            size = 1)


ggplot() + 
  geom_line(data = can_v1_sxx_dist_m_rnd1_rb %>% 
              filter(subid <= 5) %>% 
              filter(sxx == 1 & sxx_dist_m_rnd1 >= -50 & sxx_dist_m_rnd1 <= 50),
            aes(x = sxx_dist_m_rnd1,
                y = speed_kmh,
                group = subid),
            size = 3) + 
  facet_grid(.~round_txt) + 


# ggplot() + 
  geom_line(data = dat %>% 
              filter(subject_id <= 5),
            aes(x = s01_dist_m,
                y = speed_kmh,
                group = subject_id),
            colour = "green",
            size = 1) + 
  facet_grid(.~round_txt)