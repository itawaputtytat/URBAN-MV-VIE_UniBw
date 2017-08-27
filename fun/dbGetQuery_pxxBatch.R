dbGetQuery_pxxBatch <- function(db_conn,
                                sett_q = sett_query, 
                                sett_i = sett_id_names,
                                bind_rows = T, 
                                df_name = NULL,
                                add_df_name_pxx = T,
                                ...) {

  outputFunProc(R)
  ptm <- proc.time()

  ## In case of row-binding of queried data is set to TRUE (default)
  ## ... initialise object for data collection
  if (bind_rows) 
    dat_coll <- c() 
  
  invisible( 
    lapply(sett_q$pxx, function(pxx, ...) {
      
      outputString(paste0("* Fetching ", sprintf("p%02d", pxx), " ... "), linebreak = F)
      
      ## Create query string
      query <- dbCreateQueryString(pxx, sett_q, sett_i)
      
      ## Query data
      dat <- dbGetQuery(db_conn, query, stringsAsFactors = F)
      outputDone(T)
      
      ## Add query settings to dat attributes
      eval_string <- paste0("attr(dat, \"sett_query\") <- sett_q")
      eval(parse(text = eval_string))
      
      ## Add query string to dat attributes
      eval_string <- paste0("attr(dat, \"query\") <- query")
      eval(parse(text = eval_string))
      
      ## In case of row-binding of queried data is set to TRUE (default)
      ## ... row-bind data into a single data.frame
      ## ... otherwise single objects will be created
      if (bind_rows) { 
        passing <-  
          paste(sprintf("p%02d", pxx), 
                dat$round_txt, 
                sprintf("s%02d", dat[, sett_i$active$subject]),
                sep = "_")
        dat <- cbind(passing, pxx, dat, stringsAsFactors = F)
        dat <- renameVar_pxx(dat)
        dat_coll <<- rbind(dat_coll, dat, stringsAsFactors = F)
        
      } else {
        
        ## Create object name for final data
        if (add_df_name_pxx)
          df_name <- 
            paste(sett_q$src_prefix, 
                  sprintf("p%02d", pxx), 
                  sett_q$var_dist, 
                  sep = "_")
        
        assign(df_name, dat, envir = .GlobalEnv)
        outputString(paste("* New object:", df_name))
      }
    }) ## lapply
  ) ## invisible

  ## In case of row-binding of queried data is set to TRUE (default)
  ## ... assign final data to global environment
  if (bind_rows) {
    
    df_name <- paste(sett_q$src_prefix, "pxx", sett_q$var_dist, sep = "_")
    assign(df_name, dat_coll, envir = .GlobalEnv)
  
    ## Add dsett_Q to dat attributes
    eval_string <- paste0("attr(", df_name, ", \"sett_query\") <- sett_query")
    eval(parse(text = eval_string), envir = .GlobalEnv)
    
    outputString(paste("* New object:", df_name))
    outputString(paste("** (see attributes $sett_query for query settings)"))
  }
  
  
  outputProcTime(ptm)
  outputDone()
}