predLiebner_getStartVal4Sim <- function(dat2proc, set4dat, set4sim, time4sim_s = NULL) {
  
  ## Find distance value nearest to set4sim$pos4carryout based on distance
  ## Round position for carry out as workaround
  rowf.dist <- which(dat2proc[, set4dat$varname4dist_m] == round(set4sim$pos4carryout, 1))
  ## In case of several rows take the one with minimum absolute deviation
  temp <- which( abs(dat2proc[rowf.dist, set4dat$varname4dist_m] - set4sim$pos4carryout) ==
                   min(abs(dat2proc[rowf.dist, set4dat$varname4dist_m] - set4sim$pos4carryout)) )
  rowf.dist <- rowf.dist[temp]

  ## SIMULATION START
  ## Find row with corresponding time nearest to set4sim$pos4carryout minus time lag
  ## ... approximately with round
  time1 <- dat2proc[rowf.dist, set4dat$varname4time] - abs(set4sim$timelag_s)
  rowf.time <- which(round(dat2proc[, set4dat$varname4time]) == round(time1))
  ## In case of several rows take the one with minimum deviation
  temp <- which( abs(dat2proc[rowf.time, set4dat$varname4time] - time1) == 
                   min(abs(dat2proc$time_s[rowf.time] - time1)) )
  rowf.time <- rowf.time[temp]
  ## Find row with corresponding time and distance
  time1 <- dat2proc[rowf.time, set4dat$varname4time]
  dist1 <- dat2proc[rowf.time, set4dat$varname4dist_m]
  
  ## SIMULATION END
  if (is.null(time4sim_s)) {
    ## Find row with corresponding time and distance for set4sim$pos4carryout
    time2 <- dat2proc[rowf.dist, set4dat$varname4time]
    dist2 <- dat2proc[rowf.dist, set4dat$varname4dist_m]
  } else {
    ## Find row with corresponding time nearest to set4sim$pos4carryout plus time4sim_s
    ## ... approximately with round
    time2 <- dat2proc[rowf.dist, set4dat$varname4time] + time4sim_s
    rowf.time <- which(round(dat2proc[, set4dat$varname4time]) == round(time2))
    ## In case of several rows take the one with minimum deviation
    temp <- which( abs(dat2proc[rowf.time, set4dat$varname4time] - time2) == 
                     min(abs(dat2proc$time_s[rowf.time] - time2)) )
    rowf.time <- rowf.time[temp]
    ## Find row with corresponding time and distance
    time2 <- dat2proc[rowf.time, set4dat$varname4time]
    dist2 <- dat2proc[rowf.time, set4dat$varname4dist_m]
  }
  
  rowfinder1 <- which(dat2proc[, set4dat$varname4time] >= time1)
  rowfinder2 <- which(dat2proc[, set4dat$varname4time] <= time2)
  
  ## Starting values for simulation
  speed1 <- dat2proc[min(rowfinder1), set4dat$varname4speed]

  ## Get time differences for each timestamp
  time_s_diff <- dat2proc[intersect(rowfinder1, rowfinder2), set4dat$varname4time]
  #time_s_diff <- c(0, diff(time_s_diff))
  time_s_diff <- diff(time_s_diff)
  
  return(list(time1 = time1, 
              time2 = time2, 
              dist1 = dist1, 
              dist2 = dist2, 
              speed1 = speed1,
              time_s_diff = time_s_diff))
}
