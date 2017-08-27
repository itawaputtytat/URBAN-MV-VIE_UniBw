dbCreateQueryString <- function(pxx,
                                sett_q = sett_query, 
                                sett_i = sett_id_names) {

  outputFunProc(R)

  ## Create pxx as character index (e.g. 1 as "s01")
  #pxx_txt <- sprintf("s%02d", pxx)
  pxx_txt <- sprintf("p%02d", pxx)
  
  ## SELECT
  SELECT <-
    c(sett_q$var_session, 
      paste0(pxx_txt, sett_q$var_pxx),
      sett_q$var_data)
  SELECT <- paste(SELECT, collapse = ",\n")
  SELECT <- paste("SELECT", SELECT, sep = "\n")

  ## FROM
  #FROM <- paste("FROM", sett_q$src, sep = "\n")
  FROM <- 
    paste("FROM", 
          paste0(sett_q$src_prefix, "_", pxx_txt, "_", sett_q$var_dist), 
          sep = "\n")

  # ## WHERE
  WHERE_subject_id <-
    paste(paste(sett_i$active$subject, "=", sett_q$subject),
          collapse = " OR\n")
  WHERE_round_txt <-
    paste(paste0("round_txt", "= '", sett_q$round, "'"),
          collapse = " OR\n")

  ## Add buffer for selected distance criteria
  ## ... to enable correct adjustments
  ## (e.g. flawed DTI or TTI due to GPS anomalies)
  dist1_temp <- sett_q$dist1 - sett_q$dist_buffer
  dist2_temp <- sett_q$dist2 + sett_q$dist_buffer

  WHERE_dist2pxx <-
    paste(
      paste0(pxx_txt, "_", sett_q$var_dist, " >= ", dist1_temp),
      paste0(pxx_txt, "_", sett_q$var_dist, " <= ", dist2_temp),
      sep = " AND\n")

  WHERE <- c(WHERE_subject_id, WHERE_round_txt, WHERE_dist2pxx)
  WHERE <- paste0("(\n", WHERE, "\n)", collapse = " AND ")
  WHERE <- paste("WHERE", WHERE, sep = "\n")
  # WHERE <- c()
  # for(i in 1:length(sett_q$filter$sets)) {
  #   ## Extract filter criteria
  #   var_name <- sett_q$filter$sets[[1]]
  #   var_vals <- sett_q$filter$sets[[2]]
  #   comparator <- sett_q$filter$sets[[3]]
  #   bool_op_within <- 
  #     ifelse(length(sett_q$filter$sets) == 4, sett_q$filter$sets[[4]], "")
  #   
  #   ## Add quotation marks to character vector
  #   if (is.character(var_vals))
  #     var_vals <- 
  #       vapply(var_vals, function(y) paste0("\"", y, "\""), character(1))
  #   
  #   ## Merge to WHERE
  #   WHERE[i] <- 
  #     paste(paste(var_name, comparator, var_vals), 
  #           collapse = paste(" ", bool_op_within, "\n"))
  # }
  # ## Merge WHERE parts using 
  # WHERE <- 
  #   paste0("( \n", 
  #          WHERE, 
  #          collapse = 
  #            paste0("\n", ") ", sett_q$filter_bool_op_between), " \n", ")")
  # cat(WHERE, "\n")

  ## Final string
  query <- unlist(paste(SELECT, FROM, WHERE, sep = "\n", collapse = "\n\n"))

  outputDone()
  
  return(query)
}
