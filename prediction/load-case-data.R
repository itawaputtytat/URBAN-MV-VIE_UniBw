
dat_case <- 
  get(sett_case$df_name) %>% 
  filter_(paste(sett_case$col_names$id, "==", deparse(sett_case$case_id))) %>% 
  select_(.dots = unlist(sett_case$col_names, use.names = FALSE))