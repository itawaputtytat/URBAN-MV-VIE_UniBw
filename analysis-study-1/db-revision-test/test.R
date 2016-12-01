ptm <- proc.time()
dat2 <- 
  dbGetQuery(dbconn_study1, "
             SELECT * 
             FROM 
             v_adtf_dist_m_rnd1_full_s01
             WHERE 
             s01_dist_m_rnd1 >= -100 AND
             s01_dist_m_rnd1 <= 100
             ")
outputProcTime(ptm)

# ggplot() + 
# geom_line(data = dat2,
#           aes(x = s01_dist_m_rnd1,
#               y = speed_kmh,
#               group = subject_id),
#           colour = "green",
#           size = 1) + 
#   facet_grid(.~round_txt)