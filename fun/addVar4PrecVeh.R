addVar4PrecVeh <- function(dat, 
                           db_conn_name,
                           col_name_subject = "subject_id", 
                           col_name_round = "round_txt",
                           col_name_position = "pxx") {
  
  outputFunProc(R)

  name4obj <- deparseDataFunArg(dat, return_dat = F)
  dat <- deparseDataFunArg(dat)
  
  ## Find unique situations in data
  pxx_unique <- unique(dat[, col_name_position])
    
  ## Load data from database
  db_conn <- get(db_conn_name)
  dat_prec_veh <- dbGetSrc(db_conn_name, "t_pxx_preceding_vehicles")
  #dat_prec_veh <- dat_prec_veh %>% filter(round_txt %in% round2plot)
  
  ## .... and filter for unique situations
  colfinder <- grep(paste(sprintf("p%02d", pxx_unique), collapse = "|"), colnames(dat_prec_veh))
  pxx_colfinder <- colnames(dat_prec_veh)[colfinder]
  
  ## Select necessary variables
  ## ... and gather into single variable for each situation and value
  dat_prec_veh <- 
    dat_prec_veh %>% 
    select_(.dots = c(col_name_subject, col_name_round, pxx_colfinder)) %>% 
    gather_("pxx", "preceded", pxx_colfinder) %>% 
    mutate(pxx = as.numeric(sub("p", "", pxx)))
  
  ## Create additional filter
  dat_prec_veh <- 
    dat_prec_veh %>%
    mutate(filter_preceded = ifelse(preceded >= 1 &
                                      preceded <= 4,
                                    T,
                                    F)) %>%
    mutate(filter_preceded1234 = ifelse(filter_preceded == T,
                                        paste("preceded = ", preceded, sep = ""),
                                        "no relevant preceding"))
  
  ## Join original data and new preceding vehicle filter
  
  dat <- 
    left_join(dat,
              dat_prec_veh,
              by = c(col_name_subject, col_name_round, col_name_position))
  
  assign(name4obj, dat, env = .GlobalEnv)
}