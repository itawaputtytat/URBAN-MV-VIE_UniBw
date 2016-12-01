dbQueryString_adtf <- function(sxx) {

  outputFunProc(R)

  ## SELECT
  SELECT <-
    c(set4query$var_session,
      paste(sprintf("s%02d", sxx), set4query$var_sxx, sep = ""),
      set4query$var_data)
  SELECT <- paste(SELECT, collapse = ",\n")
  SELECT <- paste("SELECT", SELECT, sep = "\n")

  ## FROM
  FROM <- paste("FROM", set4query$src, sep = "\n")

  ## WHERE
WHERE_subid <- paste(paste("subid", "=", set4query$subid), collapse = " OR\n")
  WHERE_round_txt <-
    paste(paste("round_txt", "= '", set4query$round, "'", sep = ""),
          collapse = " OR\n")

  ## INSERT FILTER HERE !!!!!
  temp_dist1 <- set4query$dist1 - set4query$distpuffer
  temp_dist2 <- set4query$dist2 + set4query$distpuffer

  WHERE_dist2sxx <-
    paste(
      paste(sprintf("s%02d", sxx), "_", set4query$distvar, " >= ",
            temp_dist1, sep = ""),
      paste(sprintf("s%02d", sxx), "_", set4query$distvar, " <= ",
            temp_dist2, sep = ""),
      sep = " AND\n")

  WHERE <- c(WHERE_subid, WHERE_round_txt, WHERE_dist2sxx)
  WHERE <- paste("(\n", WHERE, "\n)", collapse = " AND ", sep = "")
  WHERE <- paste("WHERE", WHERE, sep = "\n")

  ## Final string
  query <- unlist(paste(SELECT, FROM, WHERE, collapse = "\n\n"))

  outputDone()
  return(query)
}
