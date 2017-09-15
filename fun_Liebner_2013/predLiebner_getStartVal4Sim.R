predLiebner_getStartVal4Sim <- function(dat2proc, set4dat, set4sim_temp, timelag_s = -1, time4sim_s = NULL) {

  pos4carryout_m <- set4sim_temp$dist2
  
  #dat2proc[, set4dat$col_name_am] <- round(dat2proc[, set4dat$col_name_am], 1)
  #dat2proc[, set4dat$col_name_time] <- round(dat2proc[, set4dat$col_name_time])

  ## Find distance value nearest to pos4carryout based on distance
  ## Round position for carry out as workaround
  #rowf.dist <- which(round(dat2proc[, set4dat$col_name_am]) == round(pos4carryout_m))
  ## In case of several rows take the one with minimum absolute deviation
  # temp <- which( abs(dat2proc[rowf.dist, set4dat$col_name_am] - pos4carryout_m) ==
  #                  min(abs(dat2proc[rowf.dist, set4dat$col_name_am] - pos4carryout_m)) )
  rowf.dist <- 
    which.min( abs(dat2proc[, set4dat$col_name_am] - pos4carryout_m) )
  #rowf.dist <- rowf.dist[temp]

  ## SIMULATION START
  ## Find row with corresponding time nearest to pos4carryout minus time lag
  ## ... approximately with round
  time1 <- dat2proc[rowf.dist, set4dat$col_name_time] - abs(timelag_s)
  rowf.time <- which(round(dat2proc[, set4dat$col_name_time]) == round(time1))
  
  if (length(rowf.time) != 0) {
    ## In case of several rows take the one with minimum deviation
    temp <- which.min( abs(dat2proc[rowf.time, set4dat$col_name_time] - time1) )
    rowf.time <- rowf.time[temp]
    ## Find row with corresponding time and distance
    time1 <- dat2proc[rowf.time, set4dat$col_name_time]
    dist1 <- dat2proc[rowf.time, set4dat$col_name_am]
  } else {
    #stop("ERROR")
    rowf.time <- which.min( abs(dat2proc[, set4dat$col_name_time] - time1) )
    time1 <- dat2proc[rowf.time, set4dat$col_name_time]
    dist1 <- dat2proc[rowf.time, set4dat$col_name_am]
  }
  
  ## SIMULATION END
  if (is.null(time4sim_s)) {
    ## Find row with corresponding time and distance for pos4carryout
    time2 <- dat2proc[rowf.dist, set4dat$col_name_time]
    dist2 <- dat2proc[rowf.dist, set4dat$col_name_am]
  } else {
    ## Find row with corresponding time nearest to pos4carryout plus time4sim_s
    ## ... approximately with round
    time2 <- dat2proc[rowf.dist, set4dat$col_name_time] + time4sim_s
    rowf.time <- which(dat2proc[, set4dat$col_name_time] == round(time2))
    ## In case of several rows take the one with minimum deviation
    temp <- which.min( abs(dat2proc[rowf.time, set4dat$col_name_time] - time2) )
    rowf.time <- rowf.time[temp]
    ## Find row with corresponding time and distance
    time2 <- dat2proc[rowf.time, set4dat$col_name_time]
    dist2 <- dat2proc[rowf.time, set4dat$col_name_am]
  }

  rowfinder1 <- which(dat2proc[, set4dat$col_name_time] >= time1)
  rowfinder2 <- which(dat2proc[, set4dat$col_name_time] <= time2)

  ## Starting values for simulation
  speed1 <- dat2proc[min(rowfinder1), set4dat$col_name_speed]
  
  ## Current speed
  speed2 <- dat2proc[rowf.dist, set4dat$col_name_speed]

  ## Actual driven distance
  dist_diff = dist2 - dist1
  
  ## Actual driven distance
  time_diff = time2 - time1
  
  ## Get time differences for each timestamp
  time_s_diff <- dat2proc[intersect(rowfinder1, rowfinder2), set4dat$col_name_time]
  #time_s_diff <- c(0, diff(time_s_diff))
  time_s_diff <- diff(time_s_diff)

  return(list(time1 = time1,
              time2 = time2,
              time_diff = time_diff,
              dist1 = dist1,
              dist2 = dist2,
              dist_diff = dist_diff,
              speed1 = speed1,
              speed2 = speed2,
              time_s_diff = time_s_diff))
}


