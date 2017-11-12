correctPositionAnomamlies(sett_query$df_name,
                          db_conn_name = sett_query$db_conn_name)

cut2dist_batch4rb(paste_(sett_query$df_name),
                  sett_query$col_name_am,
                  sett_query$am_limit1,
                  sett_query$am_limit2)
