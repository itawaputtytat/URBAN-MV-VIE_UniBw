predLiebner_getStartVal4Sim <- function(dat2proc, set4dat, pos4carryout_m, timelag_s = -1, time4sim_s = NULL) {

  #dat2proc[, set4dat$varname4sxx_dist_m] <- round(dat2proc[, set4dat$varname4sxx_dist_m], 1)
  #dat2proc[, set4dat$varname4time_s] <- round(dat2proc[, set4dat$varname4time_s])

  ## Find distance value nearest to pos4carryout based on distance
  ## Round position for carry out as workaround
  #rowf.dist <- which(round(dat2proc[, set4dat$varname4sxx_dist_m]) == round(pos4carryout_m))
  ## In case of several rows take the one with minimum absolute deviation
  # temp <- which( abs(dat2proc[rowf.dist, set4dat$varname4sxx_dist_m] - pos4carryout_m) ==
  #                  min(abs(dat2proc[rowf.dist, set4dat$varname4sxx_dist_m] - pos4carryout_m)) )
  rowf.dist <- which( abs(dat2proc[, set4dat$varname4sxx_dist_m] - pos4carryout_m) ==
                  min(abs(dat2proc[, set4dat$varname4sxx_dist_m] - pos4carryout_m)) )
  #rowf.dist <- rowf.dist[temp]

  ## SIMULATION START
  ## Find row with corresponding time nearest to pos4carryout minus time lag
  ## ... approximately with round
  time1 <- dat2proc[rowf.dist, set4dat$varname4time_s] - abs(timelag_s)
  rowf.time <- which(round(dat2proc[, set4dat$varname4time_s]) == round(time1))
  
  if (length(rowf.time) != 0) {
    ## In case of several rows take the one with minimum deviation
    temp <- which( abs(dat2proc[rowf.time, set4dat$varname4time_s] - time1) ==
                     min(abs(dat2proc[rowf.time, set4dat$varname4time_s] - time1)) )
    rowf.time <- rowf.time[temp]
    ## Find row with corresponding time and distance
    time1 <- dat2proc[rowf.time, set4dat$varname4time_s]
    dist1 <- dat2proc[rowf.time, set4dat$varname4sxx_dist_m]
  } else {
    #stop("ERROR")
    rowf.time <- which( abs(dat2proc[, set4dat$varname4time_s] - time1) == 
                     min(abs(dat2proc[, set4dat$varname4time_s] - time1)) )
    time1 <- dat2proc[rowf.time, set4dat$varname4time_s]
    dist1 <- dat2proc[rowf.time, set4dat$varname4sxx_dist_m]
  }
  
  ## SIMULATION END
  if (is.null(time4sim_s)) {
    ## Find row with corresponding time and distance for pos4carryout
    time2 <- dat2proc[rowf.dist, set4dat$varname4time_s]
    dist2 <- dat2proc[rowf.dist, set4dat$varname4sxx_dist_m]
  } else {
    ## Find row with corresponding time nearest to pos4carryout plus time4sim_s
    ## ... approximately with round
    time2 <- dat2proc[rowf.dist, set4dat$varname4time_s] + time4sim_s
    rowf.time <- which(dat2proc[, set4dat$varname4time_s] == round(time2))
    ## In case of several rows take the one with minimum deviation
    temp <- which( abs(dat2proc[rowf.time, set4dat$varname4time_s] - time2) ==
                     min(abs(dat2proc[rowf.time, set4dat$varname4time_s] - time2)) )
    rowf.time <- rowf.time[temp]
    ## Find row with corresponding time and distance
    time2 <- dat2proc[rowf.time, set4dat$varname4time_s]
    dist2 <- dat2proc[rowf.time, set4dat$varname4sxx_dist_m]
  }

  rowfinder1 <- which(dat2proc[, set4dat$varname4time_s] >= time1)
  rowfinder2 <- which(dat2proc[, set4dat$varname4time_s] <= time2)

  ## Starting values for simulation
  speed1 <- dat2proc[min(rowfinder1), set4dat$varname4speed_ms]

  ## Get time differences for each timestamp
  time_s_diff <- dat2proc[intersect(rowfinder1, rowfinder2), set4dat$varname4time_s]
  #time_s_diff <- c(0, diff(time_s_diff))
  time_s_diff <- diff(time_s_diff)

  return(list(time1 = time1,
              time2 = time2,
              dist1 = dist1,
              dist2 = dist2,
              speed1 = speed1,
              time_s_diff = time_s_diff))
}


