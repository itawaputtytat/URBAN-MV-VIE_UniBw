dbConnect_operator <- function() {
  outputFunProc(R)

  ## Adjust settings
  outputString(paste("Select study ", "\n",
                     paste(set4db$prompt, collapse = "\n"),
                     sep = ""))
  set4db$input <- as.numeric(readline(">>> "))
  set4db$name <- paste(set4db$dns, set4db$select[set4db$input], sep = "")
  set4db$name_short <- tolower(set4db$select[set4db$input])
  outputString(paste("* Connecting to:", set4db$name))
  set4db$pwd <- readline(">>> Enter password: ")
  #set4db <<- set4db
  set4db <<- set4db

  ## Connect to database
  dbConnect_set4db(set4db, set4db$name_short)
  outputDone()
}
