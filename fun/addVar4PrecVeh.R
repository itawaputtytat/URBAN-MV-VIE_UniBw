addVar4PrecVeh <- function(dat2proc, 
                           varname4subject = "subject_id", 
                           varname4round = "round_txt",
                           varname4pxx = "pxx") {
  
  outputFunProc(R)

  ## Find unique situations in data
  pxx_unique <- unique(dat2proc[, varname4pxx])
    
  ## Load data from database
  dat4prec <- dbGetSrc("db_conn_8", "t_pxx_preceding_vehicles")
  #dat4prec <- dat4prec %>% filter(round_txt %in% round2plot)
  
  ## .... and filter for unique situations
  colfinder <- grep(paste(sprintf("p%02d", pxx_unique), collapse = "|"), colnames(dat4prec))
  pxx_colfinder <- colnames(dat4prec)[colfinder]
  
  ## Select necessary variables
  ## ... and gather into single variable for each situation and value
  dat4prec <- 
    dat4prec %>% 
    select_(.dots = c(varname4subject, varname4round, pxx_colfinder)) %>% 
    gather_("pxx", "preceded", pxx_colfinder) %>% 
    mutate(pxx = as.numeric(sub("p", "", pxx)))
  
  ## Create additional filter
  dat4prec <- 
    dat4prec %>%
    mutate(filter_preceded = ifelse(preceded >= 1 &
                                      preceded <= 4,
                                    T,
                                    F)) %>%
    mutate(filter_preceded1234 = ifelse(filter_preceded == T,
                                        paste("preceded = ", preceded, sep = ""),
                                        "no relevant preceding"))
  
  ## Join original data and new preceding vehicle filter
  dat2proc <- 
    left_join(dat2proc,
              dat4prec,
              by = c(varname4subject, varname4round, varname4pxx))
  
  return(dat2proc)
}