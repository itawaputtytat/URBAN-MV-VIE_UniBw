predLiebner_findSimStartValues <- function(dat, 
                                           am_carryout,
                                           col_name_am,
                                           col_name_time,
                                           col_name_speed,
                                           time_lag_s, 
                                           time_sim_s = NULL) {
  
  ## Find am value that is the nearest to am_carryout user selection
  row_finder_am <- which.min( abs(dat[, col_name_am] - am_carryout) )
  
  ## SIMULATION START
  ## ----------------
  
  ## Find row with corresponding time nearest to am_carryout minus time lag
  ## Approximately with rounded valued
  time1 <- dat[row_finder_am, col_name_time] - abs(time_lag_s)
  row_finder_time <- which(round(dat[, col_name_time]) == round(time1))
  
  ## In case of several rows take the one with minimum deviation
  if (length(row_finder_time) != 0) {
    temp <- which.min( abs(dat[row_finder_time, col_name_time] - time1) )
    row_finder_time <- row_finder_time[temp]
  } else {
    row_finder_time <- which.min( abs(dat[, col_name_time] - time1) )
  }
  
  ## Find row with corresponding time and distance
  time1 <- dat[row_finder_time, col_name_time]
  am1 <- dat[row_finder_time, col_name_am]
  
  ## SIMULATION END
  ## --------------
  
  if (is.null(time_sim_s)) {
    
    ## Find row with corresponding time and distance for am_carryout
    time2 <- dat[row_finder_am, col_name_time]
    am2 <- dat[row_finder_am, col_name_am]
    
  } else {
    
    ## Find row with corresponding time nearest to am_carryout plus time_sim_s
    ## ... approximately with round
    time2 <- dat[row_finder_am, col_name_time] + time_sim_s
    row_finder_time <- which(round(dat[, col_name_time]) == round(time2))
    ## In case of several rows take the one with minimum deviation
    temp <- which.min( abs(dat[row_finder_time, col_name_time] - time2) )
    row_finder_time <- row_finder_time[temp]
    ## Find row with corresponding time and distance
    time2 <- dat[row_finder_time, col_name_time]
    am2 <- dat[row_finder_time, col_name_am]
  }
  
  rowfinder1 <- which(dat[, col_name_time] >= time1)
  rowfinder2 <- which(dat[, col_name_time] <= time2)
  
  ## Speed value at AM where simulation starts
  speed1 <- dat[min(rowfinder1), col_name_speed]
  
  ## Speed value at AM where prediction is carried out
  speed2 <- dat[row_finder_am, col_name_speed]
  
  ## Actual driven distance
  dist_diff = am2 - am1
  
  ## Actual driven time
  time_diff = time2 - time1
  
  ## Get time differences for each timestamp
  time_s_diff <- dat[intersect(rowfinder1, rowfinder2), col_name_time]
  #time_s_diff <- c(0, diff(time_s_diff))
  time_s_diff <- diff(time_s_diff)
  
  failed <- FALSE
  ## If no date before timelag is available: Skip prediction
  if (time1 < (min(dat[, col_name_time]) - time_lag_s)) {
    
    failed <- TRUE
    messageWithSepLine(c(paste("Simulation at AM", am_carryout, "failed:"), 
                         "No data available before time lag"))
    opt <- options(show.error.messages = FALSE) 
    on.exit(options(opt)) 
    stop()
  }
  
  return(list(time1 = time1,
              time2 = time2,
              time_diff = time_diff,
              am1 = am1,
              am2 = am2,
              dist_diff = dist_diff,
              speed1 = speed1,
              speed2 = speed2,
              time_s_diff = time_s_diff,
              failed = failed))
}


