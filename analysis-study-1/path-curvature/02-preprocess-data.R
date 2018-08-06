
# Pre-process data --------------------------------------------------------

intrpldf_batch4rb(sett_query$df_name, 
                  col_name_ref = sett_query$col_name_am, 
                  binary_vars = c("brake_status"),
                  suffix = "intrpld", 
                  outputFlag = T)

correctPositionAnomalies(paste_(sett_query$df_name, "intrpld"),
                         col_name_am = sett_query$col_name_am,
                         db_conn_name = sett_query$db_conn_name)

cut2dist_batch4rb(paste_(sett_query$df_name, "intrpld"),
                  sett_query$col_name_am,
                  sett_query$am_limit1,
                  sett_query$am_limit2)

#computeVar_dist2steermax("can", "dist_m_rnd1")