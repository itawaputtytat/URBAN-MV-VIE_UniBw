renameVar_sxx2 <- function (dat2proc) {

  outputFunProc(R)

  ## Get data names in current workspace which containt pattern like "s01"
  pattern <- paste(sprintf("s%02d", 1:18), collapse = "|")
  
  ## Get old (current) variable names and rename sxx related variables
  varnames_old <- names(dat2proc)
  varnames_new <- gsub(pattern, "sxx", varnames_old)
  names(dat2proc) <- varnames_new
  
  ## Save to original object
  outputDone()
  return(dat2proc)
}
