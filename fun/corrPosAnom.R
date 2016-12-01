corrPosAnom <- function (prefix,
                            suffix,
                            sxx2corr = NA,
                            colname4ref = "sxx_dist_m_rnd1",
                            dbconn = "dbconn_study1",
                            save2src = T) {
  
  outputFunProc(R)

  ## Select cases to correct (coded with "correction" in database)
  ## ... and create column: Direction for correction (plus or minus)
  cases2corr <-
    dbGetSrc(dbconn, "v_steerangle_pos_correction") %>%
    filter(grepl("correction", consequence)) %>%
    mutate(direction = ifelse(grepl("plus", consequence), "plus", "minus"))

  ## Load data for correction (average or median positions)
  pos_stat <- dbGetSrc(dbconn, "t_sxx_steerangle_deg_max_stats")

  ## Find data names of interest and filter for needed corrections
  objnames <- findObjNames(c(prefix, suffix, "intrpl"), "cut", output = F)
  filter4sxx <- sapply(objnames, function(x)
    substr(x, nchar("can") + 3, nchar("can") + 4))
  filter4sxx <- as.numeric(filter4sxx)

  ## Pre-filter correction data depending on sxx2corr
  if (!is.na(sxx2corr))
    cases2corr <- cases2corr %>% filter(sxx %in% sxx2corr) else
      ## Otherwise: Filter by relevant objectnames
      cases2corr <- cases2corr %>% filter(sxx %in% filter4sxx)

  if (nrow(cases2corr) == 0) stop("No cases to be corrected")
  
  for(rownr in 1:nrow(cases2corr)) {
    
    ## Get meta info
    s <- cases2corr$sxx[rownr]
    r <- cases2corr$round_txt[rownr]
    sid <- cases2corr$subid[rownr]
    
    ## Load situation specific data
    objname2proc <- objnames[grepl(sprintf("s%02d", s), objnames)]
    outputString(paste("* Processing:", objname2proc))
    dat2proc <- get(objname2proc, env = .GlobalEnv)
    
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
      as.numeric()
    outputString(paste("** Median position for steerangle_deg_max:", pos_stat_temp))
    
    ## Get current value of cases
    cases2corr_temp <- 
      cases2corr %>% 
      filter(sxx == s, round_txt == r, subid == sid)
    
    ## 1) Find individual position of steerangle_peak
    pos_ind <-
      cases2corr_temp %>%
      select(sxx_dist_m_rnd1_min) %>%
      as.numeric()
    
    ## 2) Compute deviation (depending on direction)
    pos_ind_dev <- abs(pos_ind - pos_stat_temp)
    if (cases2corr_temp$direction == "minus")
      pos_ind_dev <- pos_ind_dev * -1
    rowfinder <- which(dat2proc$subid == sid & dat2proc$round_txt == r)
    
    ## 3) Shift values
    dat2proc[rowfinder, colname4ref] <- 
      dat2proc[rowfinder, colname4ref] + pos_ind_dev
    
    ## Assign changes to old dataname
    if (save2src == T) assign(objname2proc, dat2proc, env = .GlobalEnv)
    
  }
  
  if (save2src == F) return(dat2proc)
  
  # for(s in unique(cases2corr$sxx)) {
  #   round_list <- unique(cases2corr$round_txt[cases2corr$sxx == s])
  # 
  #   ## Compute individual correction
  #   for(r in round_list) {
  #     
  #     subid_list <- unique(cases2corr$subid[cases2corr$sxx == s &
  #                                             cases2corr$round_text == r])
  #     
  #     for(s in subid_list) {
  # 
  #       cat("subid", sid, "\n")
  #       cat("round:", r, "\n")
  #       cat("\n")
  #       
  #      
  #     } ## subid
  #   } ## round_txt


  #} ## sxx

}
