outputFunProc(I, "dbQueryLoop")

dbQueryLoop <- function(dbconn, set4query, ...) {

  outputFunProc(R)
  ptm <- proc.time()

  invisible( lapply(set4query$sxx, function(sxx, ...) {
    ## Create query
    query <- dbQueryString(sxx)
    ## Create object name
    name4obj <- paste(sprintf("s%02d", sxx), set4query$distvar, sep = "_")
    if (!is.null(set4query$save2df_prefix))
      name4obj <- paste(set4query$save2df_prefix, name4obj, sep = "_")
    ## Query data
    assign(name4obj, dbGetQuery(get(dbconn), query), envir = .GlobalEnv)
    outputString(paste("* New object:", name4obj))
  }) )

  outputProcTime(ptm)
  outputDone()
}
