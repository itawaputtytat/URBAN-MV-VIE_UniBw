
# Objective ---------------------------------------------------------------

## Add new distance variable: dist2steermax
## ... by using computed values in t_sxx_steerangle_deg_max_stats



# Output for initialising -------------------------------------------------

cat("* Function initialised: addVar4dist2steermax \n")


# Function ----------------------------------------------------------------

addVar4dist2steermax <- function(datnames_prefix,
                                 datnames_suffix) {

  cat("\n")
  cat("Running addVar4dist2steermax: \n")
  cat("----------------------------- \n")
  cat("\n")


  ## Find data names of interest ----
  datnames <- objectNamesFinder(txt2incl = c(datnames_prefix,
                                             datnames_suffix,
                                             "intrpl"))

  cat("* Dataframes going to be processed: \n")
  cat(datnames, sep = "\n")
  cat("\n")


  ## Get data for correction
  dat4corr <- dbGetSrc("t_sxx_steerangle_deg_max_stats")


  ## Loop through datnames
  for(datname in datnames) {

    ## Get data
    dat2proc <- get(datname)

    ## Get situation number
    sxx <- substr(datname,
                  nchar(datnames_prefix) + 3,
                  nchar(datnames_prefix) + 2 + 2)
    sxx <- as.numeric(sxx)

    ## Find value for correction
    dist4corr <- dat4corr[which(dat4corr$sxx == sxx), "sxx_dist_m_rnd1_min_avg"]

    ## Find variable name with distance values
    col4distfinder <-
      grepl(paste("sxx_", datnames_suffix, sep = ""), names(dat2proc))

    ## Create new variable
    dat2proc$dist2steermax <- dat2proc[, col4distfinder] - dist4corr

    assign(datname, dat2proc, env = .GlobalEnv)

  } ## End of loop through datnames


}
