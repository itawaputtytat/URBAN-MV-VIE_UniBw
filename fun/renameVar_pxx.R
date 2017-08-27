renameVar_pxx <- function (dat) {

  outputFunProc(R)
  outputString("* Renaming pxx related variables")

  ## Get data names in current workspace which contain pattern like "s01"
  pattern <- paste(sprintf("p%02d", 1:18), collapse = "|")
  
  ## Get old (current) variable names and rename sxx related variables
  varnames_old <- names(dat)
  varnames_new <- gsub(pattern, "pxx", varnames_old)
  names(dat) <- varnames_new
  
  outputDone()
  
  return(dat)
}
