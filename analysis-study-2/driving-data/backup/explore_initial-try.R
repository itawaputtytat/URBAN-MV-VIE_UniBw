ggplot() + 
  geom_line(data = dat_study2_t_adtf_pxx_full_aggr_dti_rnd1,
            aes(x = dti_m_rnd1,
                y = speed_kmh,
                group = passing)) + 
  facet_grid(pxx~round_id)

ggplot() + 
  geom_boxplot(data = dat_study2_t_adtf_pxx_full_aggr_dti_rnd1 %>% 
                 group_by(pxx, round_id, subject_id) %>% 
                 summarize(acc_lat_ms2_max = max(acc_lat_ms2)),
            aes(x = 1,
                y = acc_lat_ms2_max)) + 
  facet_grid(pxx~round_id, scales = "free")

ggplot() + 
  geom_boxplot(data = dat_study2_t_adtf_pxx_full_aggr_dti_rnd1 %>% 
                 group_by(pxx, round_id, subject_id) %>% 
                 summarize(acc_lon_ms2_max = max(acc_lon_ms2)) %>% 
                 ungroup() %>% 
                 mutate(acc_lon_ms2_max_scaled = scale(acc_lon_ms2_max, center = F)),
               aes(x = 1,
                   y = acc_lon_ms2_max_scaled)) + 
  facet_grid(pxx~round_id, scales = "free")

ggplot() + 
  geom_boxplot(data = dat_study2_t_adtf_pxx_full_aggr_dti_rnd1 %>% 
                 filter(pxx %in% c(2, 3)) %>% 
                 group_by(pxx, round_id, subject_id) %>% 
                 summarize(acc_lon_ms2_max = max(acc_lon_ms2)) %>% 
                 ungroup() %>% 
                 mutate(acc_lon_ms2_max_scaled = scale(acc_lon_ms2_max, center = F)),
               aes(x = round_id,
                   y = acc_lon_ms2_max_scaled)) + 
  facet_grid(pxx~round_id, scales = "free")


ggplot() + 
  geom_boxplot(data = dat_study2_t_adtf_pxx_full_aggr_dti_rnd1 %>% 
                 filter(pxx %in% c(2, 3)) %>% 
                 group_by(pxx, round_id, subject_id) %>% 
                 summarize(acc_lat_ms2_max = max(acc_lat_ms2)),
               aes(x = round_id,
                   y = acc_lat_ms2_max)) + 
  facet_grid(pxx~round_id, scales = "free")
