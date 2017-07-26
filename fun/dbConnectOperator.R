dbConnectOperator <- function(settings = set4db, 
                              dbname = NULL,
                              dbconn_name = NULL,
                              disconnect_default = T) {
  outputFunProc(R)

  # ## For Markdown processing
  # if (!is.null(dbname) & !is.null(dbname)) {
  #   print("Markdown")
  #   set4db$name <- paste0(set4db$dns, dbname)
  #   set4db$name_short <- tolower(dbname)
  #   #set4db$pwd <- "WRITE-PASSWORD-HERE"
  #   set4db <<- set4db
  #   dbConnectBySettings(set4db, set4db$name_short)
  # 
  # } else { 
  #   
  #   ## Select database
  #   outputString(paste("Select study ", "\n",
  #                      paste(set4db$prompt, collapse = "\n"),
  #                      sep = ""))
  #   
  #   set4db$input <- as.numeric(readline(">>> "))
  #   set4db$name <- paste0(set4db$dns, set4db$select[set4db$input])
  #   set4db$name_short <- tolower(set4db$select[set4db$input])
  #   outputString(paste("* Connecting to:", set4db$name))
  #   set4db$pwd <- readline(">>> Enter password: ")
  #   #set4db <<- set4db
  #   set4db <<- set4db
  # 
  #   ## Connect to database
  #   dbConnectBySettings(set4db, set4db$name_short)
  #   outputDone()
  # }
  
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
  }

  if (is.null(dbconn_name))
    dbconn_name <- dbname
    
  ## Connect to target database
  pwd <- readline(">>> Enter password: ")
  dbConnectBySettings(settings, 
                      dbname = dbname, 
                      dbconn_name = dbconn_name,
                      pwd = pwd)
  
  ## Add dbname to connection attributes
  eval_string <- 
    paste0("attr(", dbconn_name, ", \"dbname\") <- \"", dbname, "\"")
  eval(parse(text = eval_string), envir = .GlobalEnv)
}
