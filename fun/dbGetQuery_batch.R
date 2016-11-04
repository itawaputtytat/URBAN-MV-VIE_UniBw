outputFunProc(I, "dbQueryLoop")

dbGetQuery_batch <- function(dbconn, set4query, rb = T, ...) {

  outputFunProc(R)
  ptm <- proc.time()

  if (rb)
    datcoll <- c()
  
  invisible( lapply(set4query$sxx, function(sxx, ...) {
    
    outputString(paste("* Processing sxx:", sxx))
    ## Create query
    query <- dbQueryString(sxx)
    ## Create object name
    name4obj <- paste(sprintf("s%02d", sxx), set4query$distvar, sep = "_")
    if (!is.null(set4query$save2df_prefix))
      name4obj <- paste(set4query$save2df_prefix, name4obj, sep = "_")
    ## Query data
    dat2proc <- dbGetQuery(get(dbconn), query, stringsAsFactors = F)
    
    if (rb) {
      passing <-  paste(sprintf("s%02d", sxx), 
                        dat2proc$round_txt, 
                        sprintf("subid%02d", dat2proc$subid),
                        sep = "_")
      dat2proc <- cbind(passing, sxx, dat2proc)
      dat2proc <- renameVar_sxx2(dat2proc)
      datcoll <<- rbind(datcoll, dat2proc)
    } else {
      assign(name4obj, dat2proc, envir = .GlobalEnv)
      outputString(paste("* New object:", name4obj))
    }
  }) )

  outputProcTime(ptm)
  outputDone()
  if (rb) {
    name4obj <- 
      paste(set4query$save2df_prefix, "sxx", set4query$distvar, "rb", sep = "_")
    assign(name4obj, datcoll, envir = .GlobalEnv)
  }
}
