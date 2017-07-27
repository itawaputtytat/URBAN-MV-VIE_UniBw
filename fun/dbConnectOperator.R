dbConnectOperator <- function(settings = set4db, 
                              dbname = NULL,
                              dbconn_name = NULL,
                              disconnect_default = T) {
  outputFunProc(R)

  ## User selection of requires database
  if (is.null(dbname)) {
    
    ## Connect to standard database
    catWithSepLine("Fetching list of databases ...")
    pwd <- readline(">>> Enter password: ")
    invisible(
      dbConnectBySettings(settings, 
                          dbname = settings$dns,
                          dbconn_name = settings$dns, 
                          pwd = pwd)
    )
    
    ## List available databases
    dblist <- 
      dbGetQuery(get(settings$dns), 
                 "SELECT datname FROM pg_database WHERE datistemplate = FALSE")
    dblist <- unlist(dblist, use.names = F)
    dblist_string <- paste(paste("[", 1:length(dblist), "]", sep = ""),  dblist)
    
    ## Disconnect default database
    invisible(
      if (disconnect_default) {
        objname <- ls(envir = .GlobalEnv, pattern = settings$dns)
        eval_string <- paste0("dbDisconnect(", objname, ")")
        eval(parse(text = eval_string))
        if (exists(objname)) {
          eval_string <- paste0("rm(", objname, ", envir = .GlobalEnv)")
          eval(parse(text = eval_string))
        }
      }
    )
    
    input <- c()
    while(T) {
      catWithSepLine("Select database:")
      outputString(paste(dblist_string, collapse = "\n"))
      input <- readline(">>> ")
      input <- as.numeric(input)
      if (!input %in% 1:length(dblist)) 
        message("Please enter the list number of the required database") else
          break;
    }
    dbname <- dblist[input]
    catWithSepLine(c("Selected database:", paste0("* ", dbname)))
    
    if (is.null(dbconn_name)) 
      dbconn_name <- paste0("dbconn_", input)
  } ## End of user selection

  ## Dynamic naming using numbers
  if (is.null(dbconn_name)) {
    dbconn_nr <- length(grep("dbconn", ls(envir = .GlobalEnv)))
      dbconn_name <- paste0("dbconn_", dbconn_nr + 1)
  }
    
    
  ## Password
  if(settings$pwd == "WRITE-PASSWORD-HERE")
    pwd <- readline(">>> Enter password: ") else
      pwd <- set4db$pwd
  
  ## Connect to target database
  dbConnectBySettings(settings, 
                      dbname = dbname, 
                      dbconn_name = dbconn_name,
                      pwd = pwd)
  
  ## Add dbname to connection attributes
  eval_string <- 
    paste0("attr(", dbconn_name, ", \"dbname\") <- \"", dbname, "\"")
  eval(parse(text = eval_string), envir = .GlobalEnv)
}
