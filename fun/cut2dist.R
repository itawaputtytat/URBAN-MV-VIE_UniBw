cut2dist <- function(prefix,
                     suffix,
                     colname4ref = "sxx_dist_m_rnd1",
                     dist1 = "ind",
                     dist2,
                     distpuffer = 50) {

  outputFunProc(R)
  outputString(paste("* Using distance variable:", colname4ref))

  ## Find object names
  objnames <- findObjNames(c(prefix, suffix, "intrpl"), "cut")

  dat <- invisible( lapply(objnames, function (objname) {

    outputString(paste("** Processing", objname))

    ## Extract number for situation out of data frame name string
    sxx <- substr(objname, nchar(prefix)+3, nchar(prefix)+3+1)
    sxx <- as.numeric(sxx)

    ## In case of individual distances
    if (grepl("ind", dist1)) {
      remember_ind = T
      rowfinder <- which(t_sxx_critdist$sxx == sxx)
      dist1 <- t_sxx_critdist$final[rowfinder] * (-1)
      ## In case of standard distances just remember the setting
      ## (will be needed at the end)
    } else {
      remember_ind = F
    }

    ## Get data
    dat <- get(objname)

    ## Remember old distance values for Output
    dist1_old <- min(dat[, colname4ref])
    dist2_old <- max(dat[, colname4ref])

    ## Identify rows
    rowfinder <- which(dat[, colname4ref] >= dist1 &
                         dat[, colname4ref] <= dist2)
    dat <- dat[rowfinder, ]

    outputString(paste("*** before:", dist1_old, "-", dist2_old))
    outputString(paste("*** now:   ", dist1, "-", dist2))

    ## Save to global environment
    name2save <- paste(objname, "cut", sep = ".")
    assign(name2save, dat, envir = .GlobalEnv)
    outputString(paste("*** Saved into:", name2save))

    if (remember_ind) dist1 <- "ind"
    }) ) %>% bind_rows()
  outputDone()
}
