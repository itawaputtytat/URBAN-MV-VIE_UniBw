intrpldf_batch4rb(sett_query$df_name, 
                  col_name_ref = sett_query$col_name_am, 
                  binary_vars = c("glance_dir_level_v2", "glance_dur_s"),
                  suffix = "intrpld", 
                  outputFlag = T)

corrPosAnom_batch4rb(paste_(sett_query$df_name, "intrpld"),
                     colname4ref = sett_query$col_name_am,
                     dbconn = sett_query$db_conn_name)

cut2dist_batch4rb(paste_(sett_query$df_name, "intrpld"),
                  sett_query$col_name_am,
                  sett_query$am_limit1,
                  sett_query$am_limit2)