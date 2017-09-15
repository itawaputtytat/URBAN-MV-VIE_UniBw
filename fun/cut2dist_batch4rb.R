cut2dist_batch4rb <- function(dat,
                              col_name_ref = "pxx_dist_m_rnd1",
                              am1 = "ind",
                              am2,
                              distpuffer = 50,
                              suffix = "_cut") {

  outputFunProc(R)
  
  objname <- deparseDataFunArg(dat, return_dat = F)
  objname <- paste0(objname, suffix)
  dat <- deparseDataFunArg(dat)
  
  outputString(paste("* Distance variable:", col_name_ref))

  unique_pxx <- unique(dat$pxx)
  
  dat.cut <- invisible( lapply(unique_pxx, function (s) {
    
    outputString(paste("* Processing pxx:", s))
    
    ## In case of individual distances
    if (grepl("ind", am1)) {
      remember_ind = T
      rowfinder <- which(t_pxx_critdist$pxx == s)
      am1 <- t_pxx_critdist$final[rowfinder] * (-1)
      ## In case of standard distances just remember the setting
      ## (will be needed at the end)
    } else {
      remember_ind = F
    }

    ## Get data corresponding to situation
    rowfinder <- which(dat$pxx == s)
    dat <- dat[rowfinder, ]

    ## Remember old distance values for Output
    am1_old <- min(dat[, col_name_ref])
    am2_old <- max(dat[, col_name_ref])

    ## Identify rows
    rowfinder <- 
      which(dat[, col_name_ref] >= am1 & dat[, col_name_ref] <= am2)
    dat <- dat[rowfinder, ]

    outputString(paste("** before:", am1_old, "-", am2_old))
    outputString(paste("** now:   ", am1, "-", am2))

    if (remember_ind) am1 <- "ind"
    return(dat)
    }) ) %>% bind_rows()
  
  ## Save to global environment
  assign(objname, dat.cut, envir = .GlobalEnv)
  outputString(paste("** Saved into:", objname))
  outputDone()
}
