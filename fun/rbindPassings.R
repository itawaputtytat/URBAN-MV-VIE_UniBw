
# Objective ---------------------------------------------------------------

## Join all data from crossings
outputFunProc(I, "rbindPassings")

# Function ----------------------------------------------------------------

rbindPassings <- function(prefix, 
                          suffix, 
                          txt2excl = NULL, 
                          colname4ref = "sxx_dist_m_rnd1") {

  outputFunProc(R)

  ## Find object names ----
  #objnames <- findObjNames(c(prefix, suffix, "intrpl", "cut"), "rb", output = F)
  objnames <- findObjNames(c(prefix, suffix), "rb", output = F)

  outputString("* Processing:")
  dat <- invisible( lapply(objnames, function(objname) {

    outputString(paste("**", objname))

    ## Get data
    dat2proc <- get(objname)

    ## Create variables for situation identification
    ## ... using an substring from original name (e.g. "s03")
    pattern <- paste(sprintf("s%02d", 1:18), collapse = "|")
    nchar_start <- as.integer(regexpr(pattern, objname))
    nchar_length <- attributes(regexpr(pattern, objname))$match.length
    dat2proc$sxx <- substr(objname, nchar_start + 1, nchar_start + nchar_length - 1)
    dat2proc$sxx <- as.numeric(dat2proc$sxx)

    ## Create variable for individual situation identification
    ## ... using variables for situation identification and round
    dat2proc$passing <-
      paste(sprintf("s%02d", dat2proc$sxx),
            dat2proc$round_txt,
            sprintf("subid%02d", dat2proc$subid),
            sep = "_")

    return(dat2proc)
  }) ) %>% bind_rows()
  
  ## Workaround
  ## For unknown reasons, there will be an rounding error 
  dat[, colname4ref] <- round(dat[, colname4ref], 1)
  return(dat)
}
