dbCreateQueryString <- function(sxx) {

  outputFunProc(R)

  ## Create sxx as character index (e.g. 1 as "s01")
  sxx_txt <- sprintf("s%02d", sxx)
  
  ## SELECT
  SELECT <-
    c(set4query$var_session, 
      paste(sxx_txt, set4query$var_sxx, sep = ""),
      set4query$var_data)
  SELECT <- paste(SELECT, collapse = ",\n")
  SELECT <- paste("SELECT", SELECT, sep = "\n")

  ## FROM
  #FROM <- paste("FROM", set4query$src, sep = "\n")
  FROM <- 
    paste("FROM", paste(set4query$src, "_", sxx_txt, sep = ""), 
          sep = "\n")

  ## WHERE
  WHERE_subject_id <- 
    paste(paste(set4idnames$active$subject, "=", set4query$subject), 
          collapse = " OR\n")
  WHERE_round_txt <-
    paste(paste("round_txt", "= '", set4query$round, "'", sep = ""),
          collapse = " OR\n")

  ## Add buffer for selected distance criteria
  ## ... to enable correct adjustments 
  ## (e.g. flawed DTI or TTI due to GPS anomalies)
  temp_dist1 <- set4query$dist1 - set4query$distbuffer
  temp_dist2 <- set4query$dist2 + set4query$distbuffer

  WHERE_dist2sxx <-
    paste(
      paste(sxx_txt, "_", set4query$distvar, " >= ", temp_dist1, sep = ""),
      paste(sxx_txt, "_", set4query$distvar, " <= ", temp_dist2, sep = ""),
      sep = " AND\n")

  WHERE <- c(WHERE_subject_id, WHERE_round_txt, WHERE_dist2sxx)
  WHERE <- paste("(\n", WHERE, "\n)", collapse = " AND ", sep = "")
  WHERE <- paste("WHERE", WHERE, sep = "\n")

  ## Final string
  query <- unlist(paste(SELECT, FROM, WHERE, sep = "\n", collapse = "\n\n"))

  outputDone()
  
  return(query)
}
