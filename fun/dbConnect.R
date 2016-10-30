connect2db <- function() {
  outputFunProc(R)

  ## Adjust settings
  prompt_txt <-
    paste(">>> Select study ",
          paste(set4db$select, collapse = "|"),
          ": ", sep = "")
  set4db$studyselect <- readline(prompt_txt)
  set4db$name <- paste(set4db$dns, set4db$studyselect, sep = "")
  set4db$name_short <- paste("study", set4db$studyselect, sep = "")
  outputString(paste("* Connecting to:", set4db$name))
  input <- readline(">>> Enter password: ")
  set4db$pwd <- input
  set4db <<- set4db

  ## Connect to database
  dbConn(set4db, set4db$name_short)
}
