intrpldf_batch4rb(study1_t_adtf_pxx_full_dist_m_rnd1, 
                  col_name_ref = sett_query$var_dist, 
                  binary_vars = "brake_status",
                  suffix = "intrpld", 
                  outputFlag = T)

cut2dist_batch4rb(study1_t_adtf_pxx_full_dist_m_rnd1_intrpld , 
                  "pxx_dist_m_rnd1", 
                  sett_query$dist1, 
                  sett_query$dist2)

corrPosAnom_batch4rb(study1_t_adtf_pxx_full_dist_m_rnd1_intrpld, 
                     colname4ref = "pxx_dist_m_rnd1",
                     dbconn = "db_conn_8")

cut2dist_batch4rb(study1_t_adtf_pxx_full_dist_m_rnd1_intrpld , 
                  "pxx_dist_m_rnd1", 
                  sett_query$dist1, 
                  sett_query$dist2)

study1_t_adtf_pxx_full_dist_m_rnd1_intrpld_cut <- 
  addVar4PrecVeh(study1_t_adtf_pxx_full_dist_m_rnd1_intrpld_cut)

study1_t_adtf_pxx_full_dist_m_rnd1_intrpld_cut <- 
  addVar4Stopping(study1_t_adtf_pxx_full_dist_m_rnd1_intrpld_cut,
                  varname4dist = "pxx_dist_m_rnd1")