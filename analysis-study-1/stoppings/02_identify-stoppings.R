
# Preparatory settings ----------------------------------------------------

sett_dat <- c()
sett_dat$dat_name <- paste_(sett_query$df_name)
sett_dat$col_names$am <- "pxx_dti_m_rnd1"
sett_dat$col_names$case <-  "passing"
sett_dat$col_names$position <- "pxx"
sett_dat$col_names$round <- "round_txt"
sett_dat$col_names$subject <- "subject_id"

sett_dat$col_names_identifier <- c(
  sett_dat$col_names$case,
  sett_dat$col_names$position,
  sett_dat$col_names$round,
  sett_dat$col_names$subject
)

sett_proc <- c()
sett_proc$thresholds$dist1 <- 0
sett_proc$thresholds$dist2 <- 25
sett_proc$thresholds$speed_kmh <- 5
sett_proc$db_conn_name <- dbFindConnObj("Study-1")

## Create final table name for database
sett_proc$db_table_name <- 
  paste_("t_stoppings", 
         paste_(names(sett_proc$thresholds), 
                sett_proc$thresholds,
                collapse = "_"))
sett_proc$db_table_name <- sub("-", "m", sett_proc$db_table_name)



# Identify stoppings ------------------------------------------------------

dat_stoppings <- 
  identifyStoppings(get(sett_dat$dat_name),
                    col_name_am = sett_dat$col_names$am,
                    col_names_identifier = sett_dat$col_names_identifier,
                    dist1 = sett_proc$thresholds$dist1,
                    dist2 = sett_proc$thresholds$dist2,
                    speed_threshold_kmh = sett_proc$thresholds$speed_kmh,
                    return_cases_only = T)



# Write to database -------------------------------------------------------

dbWriteTable(get(sett_proc$db_conn_name),
             sett_proc$db_table_name,
             dat_stoppings,
             row.names = F,
             overwrite = T)
