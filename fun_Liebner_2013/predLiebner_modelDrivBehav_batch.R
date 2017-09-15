predLiebner_modelDrivBehav_batch <- function(algo4hypscore, 
                                             pos4carryout_m,
                                             sett_sim,
                                             sett_sim_temp,
                                             sett_dat,
                                             dat_dsm,
                                             coll4simtail) {
  
  
  #dat_sim2 <- list()
  #name4listobj <- c()
  #dummy <- rep(0, length(sett_sim_temp$time_s_diff))
  #dummy_v <- c(sett_sim_temp$speed1, dummy)
  #dummy_s <- c(sett_sim_temp$dist1, dummy)
  
  ## Use i as list id for each hypothesis
  #i = 1

  ## For each hypothesis
  for(j in 1:length(sett_sim$computeI)) {
    
    ## Get object position corresponding to intention
    objpos <- sett_sim$objpos[j]
    
    ## Check if driver did not already passed corresponding objects
    ## Otherwise do not simulate driver behaviour
    # if(j %in% c(1,3) | pos4carryout_m <= objpos) {
    #if (pos4carryout <= objpos) {
      
      ## For each speed model
      for(k in 1:length(sett_sim$v_ms.max)) {
        
        ## Get data from dsm for u
        ## For intention 1 and 2 this will be constant maximum u
        ## For intention 3 and 4 synthesised models will be used
        if (j %in% c(1, 2))
          #u <- sett_sim$v_ms.max[k] else 
          u <- tail(dat_dsm[, paste0("k", k)], 1) else
          u <- dat_dsm[dat_dsm$dist == sett_sim_temp$pos4carryout_precise, paste0("k", k)]
        
        ## For each acceleration model
        for(l in 1:length(sett_sim$acc_lon_ms2.max)) {
          
          ## Get acceleration
          acc_lon_ms2.max <- sett_sim$acc_lon_ms2.max[l]
          #a <- acc_lon_ms2.max * 0.01
          
          ## Initialise previous simulation values
          ## (Values from position of carrying out the simulation)
          #v_sim <- sett_sim_temp$speed1
          #s_sim <- sett_sim_temp$dist1
          
          #v_sim <- dummy_v
          #s_sim <- dummy_s
          #v_sim.coll <- dummy_v
          #s_sim.coll <- dummy_s
          # i = 2
          
          ## Initialise objects for following computations
          # a_sim <- 0   ## Simulated acceleration
          # gap_des <- 0 ## Desired gap
          # gap_act <- 1 ## Actual gap
          
          # v_sim.prev <- sett_sim_temp$speed1
          # s_sim.prev <- sett_sim_temp$dist1
          
          #lapply(sett_sim_temp$time_s_diff, function(dt) {
          # for(dt in sett_sim_temp$time_s_diff) {
            ## When using individual timesteps
            #a <- acc_lon_ms2.max * dt

            # ## Compute current gap values
            # if (j %in% c(2, 4)) {
            # #   gap_des <- 0
            # #   gap_act <- 1
            # # } else {
            #   gap_des <- idmGap_des(sett_idm$d0, v_sim, acc_lon_ms2.max, sett_idm$b)
            #   gap_act <- idmGap_act(s_sim, objpos)
            # }
             
            ## Compute new longitudinal behaviour
            # a_sim <- a * ( (1 - ( v_sim.prev / u)^sett_idm$delta ) - ( gap_des / gap_act)^2 )
            # v_sim.prev <<- v_sim.prev + a_sim
            # s_sim.prev <<- s_sim.prev + v_sim.prev * dt
            # a_sim <- a * ( (1 - ( v_sim / u)^sett_idm$delta ) - ( gap_des / gap_act)^2 )
            
            ## When using lapply
            #v_sim <<- v_sim + a_sim
            #s_sim <<- s_sim + v_sim * dt
            ## When using for loop
            # v_sim <- v_sim + a_sim
            # s_sim <- s_sim + v_sim * dt
            #temp <- c()
          #if(j %in% c(1,3) | pos4carryout_m <= objpos) {
            temp <- sim_asv(sett_sim_temp$speed1, 
                            u, 
                            sett_idm$delta, 
                            sett_sim_temp$dist1, 
                            sett_idm$d0, 
                            acc_lon_ms2.max, 
                            sett_idm$b, 
                            objpos, 
                            j,
                            sett_sim_temp$pos4carryout_precise,
                            sett_sim_temp$time_s_diff)
                            #sett_sim$acc_lon_ms2.max)

          #}
          #else {
          #  temp$v_sim <- NULL
          #  temp$s_sim <- NULL
          #}
            # v_sim <- temp$v_sim
            # s_sim <- temp$s_sim
            # rm(temp)
            # gc()

            # v_sim.coll[i] <- v_sim
            # s_sim.coll[i] <- s_sim
            # i = i + 1
          #})
          # }
          
          #dat_sim2 <- append(dat_sim2, list(data.frame(dist_m = s_sim.coll, speed_ms = v_sim.coll)))
            #dat_sim2 <- append(dat_sim2, list(data.frame(dist_m = s_sim, speed_ms = v_sim)))
            current <- paste0(c("j", "k", "l"), c(j, k, l), collapse = "_")
            #print(current)
            #print(u)
            #dat_sim2 <- append(dat_sim2, list(data.frame(dist_m = temp$s_sim, speed_ms = temp$v_sim)))
            #coll4simtail[[current]] <- data.frame(dist_m = temp$s_sim, speed_ms = temp$v_sim)
            #coll4simtail[[current]] <- c(temp$s_sim, temp$v_sim)
            coll4simtail[[current]] <- temp
            #temp$hyp <- paste(c("j", "k", "l"), c(j, k, l), collapse = "_", sep = "")
            #dat_sim2 <- data.table::rbindlist(list(dat_sim2, temp))
          # 
             # name4listobj <-
             #   c(name4listobj,
             #     paste(c("j", "k", "l"), c(j, k, l), collapse = "_", sep = ""))
          # names(dat_sim2) <- name4listobj
          ## Save values for each hypothesis in list
          ## Create name
          #name <- paste(c("j", "k", "l"), c(j, k, l), collapse = "_", sep = "")
          #coll4simtail[[name]] <- data.frame(dist_m = s_sim, speed_ms = v_sim)

          #coll4simtail[[name]] <- data.frame(dist_m = temp$s_sim, speed_ms = temp$v_sim)
          #coll4simtail[[i]] <- data.frame(dist_m = s_sim, speed_ms = v_sim)
          #i <- i + 1
          #coll4simtail[[name]] <- data.frame(dist_m = s_sim.coll, speed_ms = v_sim.coll)

        } ## Acceleration model
      } ## Speed model
      
    #} #else { ## When driver already passed the object
      #j <- j + 1
      #i <- i + 9
      #outputString(paste("Driver already passed object in intention", j))
    #  i = i + 9
    #}
  } ## Intent
  
  #names(dat_sim2) <- name4listobj
  #return(dat_sim2)
  return(coll4simtail)
}

# ## For each hypothesis
# for(j in 1:length(sett_sim$computeI)) { 

# ## For each speed model
# for(k in 1:length(sett_sim$v_ms.max)) { 
#   
#   ## Initialise u
#   if (j %in% c(1, 2)) {
#     dat_u <- rep(sett_sim$v_ms.max[k], nrow(dat_sim))
#     u <- dat_u[1]
#   } else {
#     dat_u <- 
#       #dat_dsm.spread %>% 
#       # dat_dsm.spread_v2 %>% 
#       dat_dsm %>% 
#       #filter(dist %in% dat_sim$sxx_dist_m_rnd1) %>% 
#       filter(dist == sett_sim$pos4carryout) %>% 
#       data.frame()
#     dat_u <- dat_u[, paste("k", k, sep = "")]
#     dat_u <- as.numeric(dat_u)
#     dat_u <- as.vector(dat_u)
#     
#     # ## Choose u as desired velocity at position of carry-out
#     rowfinder <- which(dat_dsm.spread$dist == sett_sim$pos4carryout)
#     u <- dat_dsm.spread[rowfinder, paste("k", k, sep = "")]
#     u <- as.numeric(u)
#   }
#   # print(u)

## Initialise data
#dat_sim_temp <- dat_sim

#     ## For each acceleration model
#     for(l in 1:length(sett_sim$acc_lon_ms2.max)) {
#       
#       ## Get acceleration
#       acc_lon_ms2.max <- sett_sim$acc_lon_ms2.max[l]
#       
#       ## Create variable names
#       suffix <- paste(c("j", "k", "l"), c(j, k, l), collapse = "_", sep = "")
#       suffix <- paste("_sim", suffix, sep = "_")
#       varname4speed_sim <- paste(sett_dat$varname4speed, suffix, sep = "")
#       varname4dist_sim <- paste(sett_dat$varname4dist_m, suffix, sep = "")
# 
#       ## For each row in data (excluding first row)
#       for(i in 2:nrow(dat_sim)) { 
#       
#         objpos <- sett_sim$objpos[j]
#         ## Intention 1 and 3 will always be computed
#         if(j %in% c(1,3) | sett_sim$pos4carryout <= sett_sim$objpos[j]) {
#         
#         
#         ## Adjust parameters to each time step
#         #time_s_diff <- dat_sim[i, sett_dat$varname4time] - dat_sim[i-1, sett_dat$varname4time]
#         #b <- sett_idm$b * time_s_diff
#         a <- acc_lon_ms2.max * dat_sim$time_s_diff[i]
# 
#         v_sim.prev <- dat_sim[i-1, varname4speed_sim]
#         s_sim.prev <- dat_sim[i-1, varname4dist_sim]
#         
#         ## Choose u as desired velocity from the current new position
#         # u <- dat_u[i]
#          # if(s_sim.prev <= 5)
#          #   u <- 
#          #  dat_dsm.spread %>% 
#          #  filter(dist %in% round(s_sim.prev, 1))  else
#          #    u <- 
#          #  dat_dsm.spread %>% 
#          #  filter(dist == 5)  
#               
#       
# 
#         ## Compute current gap values
#         if (j %in% c(1, 3)) {
#           gap_des <- 0
#           gap_act <- 1
#         } else {
#           gap_des <- idmGap_des(sett_idm$d0, v_sim.prev, acc_lon_ms2.max, sett_idm$b)
#           #gap_act <- idmGap_act(s_sim.prev, objpos)
#           gap_act <- idmGap_act(s_sim.prev, objpos)
#         }
#         
#         ## Compute new speed and distance
#         a_sim <- a * ( (1 - ( v_sim.prev / u)^sett_idm$delta ) - ( gap_des / gap_act)^2 )
#         v_sim <- v_sim.prev + a_sim
#         s_sim <- s_sim.prev + v_sim * dat_sim$time_s_diff[i]
#       
#         dat_sim[i, varname4speed_sim] <- v_sim
#         dat_sim[i, varname4dist_sim] <- s_sim
#         
#         } else {
#           dat_sim[i, varname4speed_sim] <- 0
#           dat_sim[i, varname4dist_sim] <- objpos
#         }
#           
#           
#       } ## Row
#       
#       
#       #dat_sim[, varname4speed_sim] <- dat_sim_temp[, sett_dat$varname4speed]
#       #dat_sim[, varname4dist_sim] <- dat_sim_temp[, sett_dat$varname4dist_m]
#       
#     } ## Acc
#   } ## Speed
# } ## Intent
#   } ## Simulation-based approach
# 
#   ## else comparison based TBD
#   #return(dat_sim)
#   return(dat_sim2)
# }




# ALT ---------------------------------------------------------------------
# 
# dat_sim <- dat_sim
# dat_sim <- left_join(dat_sim,
#                      dat_dsm.spread %>% filter(dist %in% dat_sim$sxx_dist_m_rnd1),
#                      by = setNames("dist", sett_dat$varname4dist_m))
# 
# plotdat <-
#   ggplot() +
#   geom_line(data = dat_sim, aes(x = sxx_dist_m_rnd1, y = speed_ms))
# 
# for(j in 1:4) {
#   for(k in 1:3) {
#     for(l in 1:3) {
# 
#       dat_sim_temp <- dat_sim
#       dat_sim_temp$dist2 <- 0
#       acc_lon_ms2.max <- sett_sim$acc_lon_ms2.max[l]
# 
#       for(i in 2:nrow(dat_sim)) {
# 
#         s_prev <- dat_sim[i-1, "dist2"]
#         u <- predLiebner_getu(j, k, dat_dsm.spread, s_prev)
#         v_prev <- dat_sim_temp[i-1, sett_dat$varname4speed]
# 
#         time_s_diff <- dat_sim[i, sett_dat$varname4time] - dat_sim[i-1, sett_dat$varname4time]
#         b <- sett_idm$b * time_s_diff
#         a <- acc_lon_ms2.max * time_s_diff
# 
#         if (j %in% c(1, 3)) {
#           gap_des <- 0
#           gap_act <- 1
#         } else {
#           gap_des <- idmGap_desired(sett_idm$d0, v_prev, a, b)#acc_lon_ms2.max,sett_idm$b)
#           gap_act <- idmGap_actual(s_prev, objpos)
#         }
# 
#         a_sim <- idmAcc(a, v_prev, u, sett_idm$delta, gap_des, gap_act)
#         v_sim <- v_prev + a_sim
#         s_sim <- s_prev + v_sim * time_s_diff
# 
#         dat_sim_temp[i, sett_dat$varname4speed] <- v_sim
#         dat_sim_temp[i, "dist2"] <- s_sim
#       }
# 
#       if (j == 1) c = "blue"
#       if (j == 2) c = "orange"
#       if (j == 3) c = "red"
#       if (j == 4) c = "magenta"
# 
#       plotdat <-
#         plotdat +
#         geom_line(data = dat_sim_temp, aes(x = sxx_dist_m_rnd1, y = speed_ms), col = c)
# 
#     }
#   }
# }
# 
# 
