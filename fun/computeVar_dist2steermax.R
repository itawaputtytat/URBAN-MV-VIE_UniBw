
# Objective ---------------------------------------------------------------

## Add new distance variable: dist2steermax
## ... by using computed values in t_sxx_steerangle_deg_max_stats

# Function ----------------------------------------------------------------

computeVar_dist2steermax <- function(prefix,
                                     suffix,
                                     dbconn = "dbconn_study1") {

  outputFunProc(R)

  ## Find data names of interest ----
  objnames <- findObjNames(c(prefix, suffix, "intrpl"), "rb", output = F)

  ## Get data for correction
  dat4corr <- dbGetSrc(dbconn, "t_sxx_steerangle_deg_max_stats")

  outputString("* Processing:")
  invisible( lapply(objnames, function(objname) {

    outputString(paste("**", objname))
    dat2proc <- get(objname)
    ## Get situation number
    sxx <- substr(objname, nchar(prefix) + 3, nchar(prefix) + 2 + 2)
    sxx <- as.numeric(sxx)

    ## Find value for correction
    dist4corr <- dat4corr[which(dat4corr$sxx == sxx), "sxx_dist_m_rnd1_min_avg"]

    ## Find variable name with distance values
    colfinder <- grepl(paste("sxx_", suffix, sep = ""), names(dat2proc))

    ## Compute new new variable steer max and assign to global object
    dat2proc$dist2steermax <- dat2proc[, colfinder] - dist4corr
    assign(objname, dat2proc, env = .GlobalEnv)
  }) )
  outputDone()
}
