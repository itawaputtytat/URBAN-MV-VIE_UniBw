## @knitr dbConnect_operator

dbConnect_operator <- function(dbname = NULL) {
  outputFunProc(R)

  if (!is.null(dbname) & !is.null(dbname)) {
    set4db$name <- paste0(set4db$dns, dbname)
    set4db$name_short <- tolower(dbname)
    set4db$pwd <- "keines"
    set4db <<- set4db
    dbConnect_set4db(set4db, set4db$name_short)
  } else {
    ## Adjust settings
    outputString(paste("Select study ", "\n",
                       paste(set4db$prompt, collapse = "\n"),
                       sep = ""))
    set4db$input <- as.numeric(readline(">>> "))
    set4db$name <- paste0(set4db$dns, set4db$select[set4db$input])
    set4db$name_short <- tolower(set4db$select[set4db$input])
    outputString(paste("* Connecting to:", set4db$name))
    set4db$pwd <- readline(">>> Enter password: ")
    #set4db <<- set4db
    set4db <<- set4db

    ## Connect to database
    dbConnect_set4db(set4db, set4db$name_short)
    outputDone()
  }
}
