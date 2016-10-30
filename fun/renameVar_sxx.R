renameVar_sxx <- function () {

  outputFunProc(R)

  ## Get data names in current workspace which containt pattern like "s01"
  pattern <- paste(sprintf("s%02d", 1:18), collapse = "|")
  #datnames <- objectNamesFinder(txt2incl = strings2lookfor)
  namelist <- findObjNames(pattern, ".intrpl")

  invisible( lapply(namelist, function(name) {
    ## Get data
    dat <- get(name, env = .GlobalEnv)
    ## Get old (current) variable names and rename sxx related variables
    varnames_old <- names(dat)
    varnames_new <- gsub(pattern, "sxx", varnames_old)
    names(dat) <- varnames_new
    ## Save to original object
    assign(name, dat, env = .GlobalEnv)
  }) )
  outputDone()
}
