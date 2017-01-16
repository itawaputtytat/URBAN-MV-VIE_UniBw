outputFunProc(I, "dbQueryLoop_batch")

dbGetQuery_batch <- function(dbconn, 
                             set4query, 
                             rb = T, ...) {

  outputFunProc(R)
  ptm <- proc.time()

  ## In case of row-binding of queried data is set to TRUE (default)
  ## ... initialise object for data collection
  if (rb) 
    datcoll <- c() 
  
  invisible( lapply(set4query$sxx, function(sxx, ...) {
    
    outputString(paste("* Querying sxx:", sxx))
    
    ## Create query string
    query <- dbCreateQueryString(sxx)
    
    ## Create object name for final data
    ## (either with or without prefix)
    name4obj <- paste(sprintf("s%02d", sxx), set4query$distvar, sep = "_")
    if (!is.null(set4query$save2df_prefix))
      name4obj <- paste(set4query$save2df_prefix, name4obj, sep = "_")
    
    ## Query data
    dat2proc <- dbGetQuery(dbconn, query, stringsAsFactors = F)
    
    ## In case of row-binding of queried data is set to TRUE (default)
    ## ... row-bind data into a single data.frame
    ## ... otherwise single objects will be created
    if (rb) { 
      passing <-  
        paste(sprintf("s%02d", sxx), 
              dat2proc$round_txt, 
              sprintf("subject%02d", dat2proc[, set4idnames$active$subject]),
              sep = "_")
      dat2proc <- cbind(passing, sxx, dat2proc)
      dat2proc <- renameVar_sxx2(dat2proc)
      datcoll <<- rbind(datcoll, dat2proc)
    } else {
      assign(name4obj, dat2proc, envir = .GlobalEnv)
      outputString(paste("* New object:", name4obj))
    }
    
    outputDone(T)
  }) ## lapply
  ) ## invisible

  ## In case of row-binding of queried data is set to TRUE (default)
  ## ... assign final data to global environment
  if (rb) {
    name4obj <- set4query$src
    ## Alternative
    #name4obj <- paste(set4query$save2df_prefix, "sxx", set4query$distvar, "rb", sep = "_")
    assign(name4obj, datcoll, envir = .GlobalEnv)
    outputString(paste("* New object:", name4obj))
  }
  
  outputProcTime(ptm)
  outputDone()
}
