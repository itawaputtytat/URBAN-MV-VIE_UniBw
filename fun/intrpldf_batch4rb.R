intrpldf_batch4rb <- function(dat2proc,
                              colname4ref = "sxx_dist_m_rnd1",
                              stepsize = 0.1,
                              suffix = ".intrpl",
                              outputFlag = F) {

  outputFunProc(R)

  dat2proc.intrpl <- invisible( lapply(unique(dat2proc$passing), function(p) {

    if(outputFlag)
      outputString(paste("* Processing:", p))

    ## Get data and run inteprolation
    dat_temp <- dat2proc %>% filter(passing == p)
    dat.intrpl <- intrpldf(dat_temp, colname4ref, stepsize = stepsize)
    return(dat.intrpl)

  }) ) %>% dplyr::bind_rows()
  
  objname <- paste(deparse(substitute(dat2proc)), suffix, sep = "")
  assign(objname, dat2proc.intrpl, env = .GlobalEnv)

  outputDone()
}
