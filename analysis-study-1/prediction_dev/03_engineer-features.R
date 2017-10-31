sett_feat <- c()
#sett_feat$df_name <- paste_(sett_query$df_name, "intrpld")
sett_feat$df_name <- paste_(sett_query$df_name, "intrpld", "cut")

engineerFeatures(sett_feat$df_name,
                 convert_speed_kmh_into_speed_ms = T,
                 add_prec_veh = T,
                 add_stopping = T,
                 dist1 = -80)
