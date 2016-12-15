addVar4PrecVeh <- function(dat2proc, 
                           varname4subject = "subject_id", 
                           varname4round = "round_txt",
                           varname4sxx = "sxx") {
  
  outputFunProc(R)

  ## Find unique situations in data
  sxx_unique <- unique(dat2proc[, varname4sxx])
    
  ## Load data from database
  dat4prec <- dbGetSrc("dbconn_study1", "t_sxx_precedings")
  #dat4prec <- dat4prec %>% filter(round_txt %in% round2plot)
  
  ## .... and filter for unique situations
  colfinder <- grep(paste(sprintf("s%02d", sxx_unique), collapse = "|"), colnames(dat4prec))
  varname4sxx <- colnames(dat4prec)[colfinder]
  
  ## Select necessary variables
  ## ... and gather into single variable for each situation and value
  dat4prec <- 
    dat4prec %>% 
    select_(.dots = c(varname4subject, varname4round, varname4sxx)) %>% 
    gather_("situation_id", "preceded", varname4sxx)
  
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
              by = c(varname4subject, varname4round))
  
  return(dat2proc)
}