intrpldf_batch4rb(study2_t_adtf_pxx_full_dti_rnd1, 
                  col_name_ref = sett_query$var_dist, 
                  binary_vars = "brake_status",
                  suffix = "intrpld", 
                  outputFlag = T)

cut2dist_batch4rb(study2_t_adtf_pxx_full_dti_rnd1_intrpld, 
                  "pxx_dti_m_rnd1", 
                  sett_query$dist1, 
                  sett_query$dist2)
