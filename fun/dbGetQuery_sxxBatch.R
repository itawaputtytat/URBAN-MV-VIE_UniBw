dbGetQuery_sxxBatch <- function(db_conn,
                                sett_q = sett_query, 
                                sett_i = sett_id_names,
                                bind_rows = T, 
                                df_name = NULL,
                                add_df_name_sxx = T,
                                ...) {

  outputFunProc(R)
  ptm <- proc.time()

  ## Create df name
  if (is.null(df_name))
    df_name <- sett_q$src
  
  ## In case of row-binding of queried data is set to TRUE (default)
  ## ... initialise object for data collection
  if (bind_rows) 
    dat_coll <- c() 
  
  invisible( 
    lapply(sett_q$sxx, function(sxx, ...) {
      
      outputString(paste("* Querying sxx:", sxx))
      
      ## Create query string
      query <- dbCreateQueryString(sxx, sett_q, sett_i)
      
      ## Create object name for final data (add sxx info)
      if (add_df_name_sxx)
        df_name <- paste(df_name, sprintf("s%02d", sxx), sep = "_")
      
      ## Query data
      dat <- dbGetQuery(db_conn, query, stringsAsFactors = F)
      outputDone(T)
      
      ## Add dsett_Q to dat attributes
      eval_string <- paste0("attr(dat, \"sett_query\") <- sett_q")
      eval(parse(text = eval_string))
      
      ## In case of row-binding of queried data is set to TRUE (default)
      ## ... row-bind data into a single data.frame
      ## ... otherwise single objects will be created
      if (bind_rows) { 
        passing <-  
          paste(sprintf("s%02d", sxx), 
                dat$round_txt, 
                sprintf("subject%02d", dat[, sett_i$active$subject]),
                sep = "_")
        dat <- cbind(passing, sxx, dat)
        dat <- renameVar_sxx2(dat)
        dat_coll <<- rbind(dat_coll, dat)
      } else {
        assign(df_name, dat, envir = .GlobalEnv)
        outputString(paste("* New object:", df_name))
      }
    }) ## lapply
  ) ## invisible

  ## In case of row-binding of queried data is set to TRUE (default)
  ## ... assign final data to global environment
  if (bind_rows) {
    df_name <- sett_q$src
    ## Alternative
    #df_name <- paste(sett_q$save2df_prefix, "sxx", sett_q$var_dist, "bind_rows", sep = "_")
    assign(df_name, dat_coll, envir = .GlobalEnv)
    
    ## Add dsett_Q to dat attributes
    eval_string <- paste0("attr(", df_name, ", \"sett_query\") <- sett_query")
    eval(parse(text = eval_string), envir = .GlobalEnv)
    
    outputString(paste("* New object:", df_name))
    outputString(paste("* (see attributes for query settings)"))
  }
  
  outputProcTime(ptm)
  outputDone()
}
