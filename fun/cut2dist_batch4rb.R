cut2dist_batch4rb <- function(dat,
                              col_name_ref = "pxx_dist_m_rnd1",
                              dist1 = "ind",
                              dist2,
                              distpuffer = 50,
                              suffix = "_cut") {

  outputFunProc(R)
  outputString(paste("* Distance variable:", col_name_ref))

  unique_pxx <- unique(dat$pxx)
  
  dat.cut <- invisible( lapply(unique_pxx, function (s) {
    
    outputString(paste("* Processing pxx:", s))
    
    ## In case of individual distances
    if (grepl("ind", dist1)) {
      remember_ind = T
      rowfinder <- which(t_pxx_critdist$pxx == s)
      dist1 <- t_pxx_critdist$final[rowfinder] * (-1)
      ## In case of standard distances just remember the setting
      ## (will be needed at the end)
    } else {
      remember_ind = F
    }

    ## Get data corresponding to situation
    rowfinder <- which(dat$pxx == s)
    dat <- dat[rowfinder, ]

    ## Remember old distance values for Output
    dist1_old <- min(dat[, col_name_ref])
    dist2_old <- max(dat[, col_name_ref])

    ## Identify rows
    rowfinder <- 
      which(dat[, col_name_ref] >= dist1 & dat[, col_name_ref] <= dist2)
    dat <- dat[rowfinder, ]

    outputString(paste("** before:", dist1_old, "-", dist2_old))
    outputString(paste("** now:   ", dist1, "-", dist2))

    if (remember_ind) dist1 <- "ind"
    return(dat)
    }) ) %>% bind_rows()
  
  ## Save to global environment
  objname <- paste(deparse(substitute(dat)), suffix, sep = "")
  assign(objname, dat.cut, envir = .GlobalEnv)
  outputString(paste("** Saved into:", objname))
  outputDone()
}
