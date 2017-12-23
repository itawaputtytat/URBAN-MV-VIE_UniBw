intrpldf_batch4rb(sett_query$df_name, 
                  col_name_ref = sett_query$col_name_am, 
                  binary_vars = c("brake_status"),
                  suffix = "intrpld", 
                  outputFlag = T)

computeDistanceToSteerMax(paste_(sett_query$df_name, "intrpld"))
