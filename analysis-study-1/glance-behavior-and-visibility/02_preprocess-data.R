intrpldf_batch4rb(sett_query$df_name, 
                  col_name_ref = sett_query$col_name_am, 
                  binary_vars = c("glance_dir_level_v2", "glance_dur_s"),
                  suffix = "intrpld", 
                  outputFlag = T)

correctPositionAnomalies(paste_(sett_query$df_name, "intrpld"),
                          col_name_am = sett_query$col_name_am,
                          db_conn_name = sett_query$db_conn_name)

computeDistanceToSteerMax(paste_(sett_query$df_name, "intrpld"))

cut2dist_batch4rb(paste_(sett_query$df_name, "intrpld"),
                  sett_query$col_name_am,
                  sett_query$am_limit1,
                  sett_query$am_limit2)

addInfoOnStoppings(paste_(sett_query$df_name, "intrpld", "cut"),
                   db_conn_name = sett_query$db_conn_name,
                   db_src_name = "t_stoppings_dist1_m50_dist2_0_speed_5")
