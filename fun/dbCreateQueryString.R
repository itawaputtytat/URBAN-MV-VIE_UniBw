dbCreateQueryString <- function(pxx,
                                sett_q = sett_query, 
                                sett_i = sett_id_names,
                                include_other_am = T) {

  outputFunProc(R)
  
  ## SELECT
  SELECT <- sett_q$col_names_session
  
  if (include_other_am) {
    SELECT <-
      c(SELECT, 
        paste0(createVectorForAM(sett_q$col_name_am)) )
  } else {
    SELECT <- c(SELECT, sett_q$col_name_am)
  }
  SELECT <- c(SELECT, paste0("\"", sett_q$col_names_data, "\""))
  SELECT <- paste(SELECT, collapse = ",\n")
  SELECT <- paste("SELECT", SELECT, sep = "\n")
  
  ## FROM
  pxx_txt <- sprintf("p%02d", pxx)
  FROM <- paste("FROM", paste_(sett_q$src_name_prefix, pxx_txt), sep = "\n")
  if (!is.null(sett_query$src_name_suffix))
    FROM <- paste_(FROM, sett_q$src_name_suffix)

  # ## WHERE
  if (!is.null(sett_q$subject)) {
    WHERE_subject_id <-
      paste(paste(sett_i$active$subject, "=", sett_q$subject),
            collapse = " OR\n")
  } else {
    WHERE_subject_id <- NULL
  }
  WHERE_round_id <-
    paste(paste0("round_id", "= '", sett_q$round, "'"),
          collapse = " OR\n")

  ## Add buffer for selected distance criteria
  ## ... to enable correct adjustments
  ## (e.g. flawed DTI or TTI due to GPS anomalies)
  am_limit1_temp <- sett_q$am_limit1 - sett_q$am_buffer
  am_limit2_temp <- sett_q$am_limit2 + sett_q$am_buffer

  if (include_other_am) {
    WHERE_am2pxx <-
      paste(
        paste0(sett_q$col_name_am, " >= ", am_limit1_temp),
        paste0(sett_q$col_name_am, " <= ", am_limit2_temp),
        sep = " AND\n")
  } else {
    WHERE_am2pxx <-
      paste(
        paste0("pxx", "_", sett_q$col_name_am, " >= ", am_limit1_temp),
        paste0("pxx", "_", sett_q$col_name_am, " <= ", am_limit2_temp),
        sep = " AND\n")
  }

  WHERE <- c(WHERE_subject_id, WHERE_round_id, WHERE_am2pxx)
  WHERE <- paste0("(\n", WHERE, "\n)", collapse = " AND ")
  WHERE <- paste("WHERE", WHERE, sep = "\n")

  ## Final string
  query <- unlist(paste(SELECT, FROM, WHERE, sep = "\n", collapse = "\n\n"))
  
  outputDone()
  
  return(query)
}
