engineerFeatures <- function(df_name, 
                          convert_speed_kmh_into_speed_ms = T,
                          add_prec_veh = F,
                          add_stopping = F,
                          db_conn_name = sett_query$db_conn_name,
                          col_name_am = sett_query$col_name_am,
                          dist1 = -80,
                          ...) {
  
  if (convert_speed_kmh_into_speed_ms) {
    assign(df_name, 
           get(df_name) %>% 
             mutate(speed_ms = speed_kmh / 3.6), env = .GlobalEnv)
  }
  
  if (add_prec_veh) {
    addVar4PrecVeh(df_name,
                   db_conn_name = db_conn_name,
                   ...)
  }
  
  if (add_stopping) {
    identifyStoppings(df_name,
                    col_name_am = col_name_am,
                    ...)
  }
}