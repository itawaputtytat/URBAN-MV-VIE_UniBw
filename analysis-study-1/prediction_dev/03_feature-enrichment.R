sett_feat <- c()
sett_feat$df_name <- paste_(sett_query$df_name, "intrpld", "cut")

assign(sett_feat$df_name, 
       get(sett_feat$df_name) %>% 
         mutate(speed_ms = speed_kmh / 3.6))

addVar4PrecVeh(db_conn_name = sett_query$db_conn_name,
               sett_feat$df_name)

addVar4Stopping(sett_feat$df_name,
                varname4dist = sett_query$col_name_am,
                dist1 = -80)