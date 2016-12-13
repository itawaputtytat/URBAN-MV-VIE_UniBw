corrPosAnom_batch4rb <- function (dat2proc,
                                  colname4ref = "sxx_dist_m_rnd1",
                                  dbconn = "dbconn_study1",
                                  save2src = T) {

  outputFunProc(R)
  
  objname <- deparse(substitute(dat2proc))
  sxx_unique <- unique(dat2proc$sxx)
  dec_n <- getDecimalPlaces(dat2proc[1, colname4ref])
  
  ## Select cases to correct (coded with "correction" in database)
  ## ... and create column: Direction for correction (plus or minus)
  cases2corr <-
    dbGetSrc(dbconn, "v_steerangle_pos_correction") %>%
    filter(grepl("correction", consequence)) %>%
    mutate(direction = ifelse(grepl("plus", consequence), "plus", "minus")) %>% 
    filter(sxx %in% sxx_unique)

  if (nrow(cases2corr) == 0)
    outputString("No data to be corrected") else {
      
      ## Load data for correction (average or median positions)
      pos_stat <- 
        dbGetSrc(dbconn, "t_sxx_steerangle_deg_max_stats") %>% 
        filter(sxx %in% sxx_unique)
      
      for(rownr in 1:nrow(cases2corr)) {
        
        ## Get meta info
        s <- cases2corr$sxx[rownr]
        r <- cases2corr$round_txt[rownr]
        sid <- cases2corr$subid[rownr]
        
        outputString(paste("** Processing: Situation", s, r, "subject", sid))
        
        ## Compute and remember correction value (statistical position)
        #val4corr <-
        #   pos4sxx_steer_peaks %>%
        #   filter(sxx == case_current$sxx) %>%
        #   select(sxx_dist_m_rnd1_min_avg)
        #avgpos4corr <- as.numeric(unlist(avgpos4corr))
        #cat("Average position for steerangle_deg_max:", avgpos4corr, "\n")
        pos_stat_temp <-
          pos_stat %>%
          filter(sxx == s) %>%
          select(sxx_dist_m_rnd1_min_med) %>%
          ## As correction values may have more decimal numbers than one
          mutate(sxx_dist_m_rnd1_min_med = round(sxx_dist_m_rnd1_min_med, 1)) %>% 
          as.numeric()
        outputString(paste("*** Median position for steerangle_deg_max:", pos_stat_temp))
        
        ## Get current value of cases
        cases2corr_temp <- 
          cases2corr %>% 
          filter(sxx == s, round_txt == r) %>%
          filter_(paste(set4idnames$V1$subject, "==", sid))
        
        ## 1) Find individual position of steerangle_peak
        pos_ind <-
          cases2corr_temp %>%
          select(sxx_dist_m_rnd1_min) %>%
          as.numeric()
        
        ## 2) Compute deviation (depending on direction)
        pos_ind_dev <- abs(pos_ind - pos_stat_temp)
        if (cases2corr_temp$direction == "minus") 
          pos_ind_dev <- pos_ind_dev * -1
        rowfinder <- 
          which(dat2proc$sxx == s &
                  dat2proc[, set4idnames$active$subject] == sid & 
                  dat2proc$round_txt == r)
        
        ## 3) Shift values
        dat2proc[rowfinder, colname4ref] <- 
          dat2proc[rowfinder, colname4ref] + pos_ind_dev
        
        ## Workaround for floating point errors
        dat2proc[rowfinder, colname4ref] <- round(dat2proc[rowfinder, colname4ref], dec_n)
        
      }
      ## Assign changes to old dataname
      if (save2src == T) 
        assign(objname, dat2proc, env = .GlobalEnv) else 
          return(dat2proc)
    }
}
