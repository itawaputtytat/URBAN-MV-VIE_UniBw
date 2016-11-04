cut2dist_batch4rb <- function(dat2proc,
                              colname4ref = "sxx_dist_m_rnd1",
                              dist1 = "ind",
                              dist2,
                              distpuffer = 50,
                              suffix = ".cut") {

  outputFunProc(R)
  outputString(paste("* Distance variable:", colname4ref))

  unique_sxx <- unique(dat2proc$sxx)
  
  dat.cut <- invisible( lapply(unique_sxx, function (s) {
    
    outputString(paste("* Processing sxx:", s))
    
    ## In case of individual distances
    if (grepl("ind", dist1)) {
      remember_ind = T
      rowfinder <- which(t_sxx_critdist$sxx == s)
      dist1 <- t_sxx_critdist$final[rowfinder] * (-1)
      ## In case of standard distances just remember the setting
      ## (will be needed at the end)
    } else {
      remember_ind = F
    }

    ## Get data corresponding to situation
    rowfinder <- which(dat2proc$sxx == s)
    dat <- dat2proc[rowfinder, ]

    ## Remember old distance values for Output
    dist1_old <- min(dat[, colname4ref])
    dist2_old <- max(dat[, colname4ref])

    ## Identify rows
    rowfinder <- 
      which(dat[, colname4ref] >= dist1 & dat[, colname4ref] <= dist2)
    dat <- dat[rowfinder, ]

    outputString(paste("** before:", dist1_old, "-", dist2_old))
    outputString(paste("** now:   ", dist1, "-", dist2))

    if (remember_ind) dist1 <- "ind"
    return(dat)
    }) ) %>% bind_rows()
  
  ## Save to global environment
  objname <- paste(deparse(substitute(dat2proc)), suffix, sep = "")
  assign(objname, dat.cut, envir = .GlobalEnv)
  outputString(paste("** Saved into:", objname))
  outputDone()
}
