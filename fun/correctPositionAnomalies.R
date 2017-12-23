correctPositionAnomalies <- function(dat,
                                      db_conn_name,
                                      src_name_correction_info = 
                                        "t_steer_angle_outliers",
                                      col_name_case = "passing",
                                      col_name_am = 
                                        "pxx_dti_m_rnd1",
                                      col_name_am_corr = 
                                        "pxx_dti_m_rnd1_min_vs_median",
                                      save_global = T) {
  
  outputFunProc(R)
  
  ## Get data
  dat_name <- deparseDataFunArg(dat, return_dat = F)
  dat <- deparseDataFunArg(dat)
  
  ## Select cases to correct (coded with "correction" in database)
  ## ... and create column: Direction for correction (plus or minus)
  dat_cases <- 
    dbGetSrc(db_conn_name, src_name_correction_info) %>% 
    filter_("is_outlier")
  
  case_finder <- dat_cases[, col_name_case] %in% unique(dat[, col_name_case])
  case_finder <- dat_cases[case_finder, col_name_case]
  
  if (length(case_finder) == 0) {
    outputString("* No cases to be corrected")
  } else {
    
    for (case in case_finder) {
      
      outputString(paste("* Processing:", case))
      
      ## Extract correction value
      row_finder <- dat_cases[, col_name_case] == case
      correction <- dat_cases[row_finder, col_name_am_corr]
      
      row_finder <- dat[, col_name_case] == case
      dat[row_finder, col_name_am] <- dat[row_finder, col_name_am] + correction
      
      outputString(paste("** Corrected distance:", correction))
      
    }
    
    ## Assign changes to old dataname
    if (save_global) {
      assign(dat_name, dat, env = .GlobalEnv)
    } else {
      return(dat)
    }
    
  }

  
}
