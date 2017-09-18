corrPosAnom_batch4rb <- function(dat2proc,
                                 colname4ref = "pxx_dti_m_rnd1",
                                 dbconn = "dbconn_study1",
                                 save2src = T) {

  outputFunProc(R)
  
  name4obj <- deparseDataFunArg(dat2proc, return_dat = F)
  dat2proc <- deparseDataFunArg(dat2proc)
  pxx_unique <- unique(dat2proc$pxx)
  dec_n <- getDecimalPlaces(dat2proc[1, colname4ref])
  
  ## Select cases to correct (coded with "correction" in database)
  ## ... and create column: Direction for correction (plus or minus)
  cases2corr <-
    dbGetSrc(dbconn, "v_steer_angle_pos_correction") %>%
    filter(grepl("correction", consequence)) %>%
    mutate(direction = ifelse(grepl("plus", consequence), "plus", "minus")) %>% 
    filter(position_id %in% pxx_unique)

  if (nrow(cases2corr) == 0)
    outputString("No data to be corrected") else {
      
      ## Load data for correction (average or median positions)
      pos_stat <- 
        dbGetSrc(dbconn, "t_pxx_steer_angle_deg_max_stats") %>% 
        filter(position_id %in% pxx_unique)
      
      for(rownr in 1:nrow(cases2corr)) {
        
        ## Get meta info
        p <- cases2corr$position_id[rownr]
        r <- cases2corr$round_txt[rownr]
        sid <- cases2corr$subject_id[rownr]
        
        outputString(paste("** Processing: Situation", p, r, "subject", sid))
        
        ## Compute and remember correction value (statistical position)
        #val4corr <-
        #   pos4pxx_steer_peaks %>%
        #   filter(pxx == case_current$pxx) %>%
        #   select(pxx_dti_m_rnd1_min_avg)
        #avgpos4corr <- as.numeric(unlist(avgpos4corr))
        #cat("Average position for steer_angle_deg_max:", avgpos4corr, "\n")
        pos_stat_temp <-
          pos_stat %>%
          filter(position_id == p) %>%
          select(pxx_dti_m_rnd1_min_med) %>%
          ## As correction values may have more decimal numbers than one
          mutate(pxx_dti_m_rnd1_min_med = round(pxx_dti_m_rnd1_min_med, 1)) %>% 
          as.numeric()
        outputString(paste("*** Median position for steer_angle_deg_max:", pos_stat_temp))
        
        ## Get current value of cases
        cases2corr_temp <- 
          cases2corr %>% 
          filter(position_id == p, round_txt == r) %>%
          filter_(paste(sett_id_names$active$subject, "==", sid))
        
        ## 1) Find individual position of steer_angle_peak
        pos_ind <-
          cases2corr_temp %>%
          select(pxx_dti_m_rnd1_min) %>%
          as.numeric()
        
        ## 2) Compute deviation (depending on direction)
        pos_ind_dev <- abs(pos_ind - pos_stat_temp)
        if (cases2corr_temp$direction == "minus") 
          pos_ind_dev <- pos_ind_dev * -1
        rowfinder <- 
          which(dat2proc$pxx == p &
                  dat2proc[, sett_id_names$active$subject] == sid & 
                  dat2proc$round_txt == r)
        
        ## 3) Shift values
        dat2proc[rowfinder, colname4ref] <- 
          dat2proc[rowfinder, colname4ref] + pos_ind_dev
        
        ## Workaround for floating point errors
        dat2proc[rowfinder, colname4ref] <- round(dat2proc[rowfinder, colname4ref], dec_n)
        
      }
      ## Assign changes to old dataname
      if (save2src == T) 
        assign(name4obj, dat2proc, env = .GlobalEnv) else 
          return(dat2proc)
    }
}
