predLiebner_modelDrivBehav_batch <- function(algo4hypscore, 
                                             pos4carryout_m,
                                             set4sim,
                                             set4dat,
                                             dat4sim,
                                             dat4dvm) {
  
    dat4sim2 <- list()
    name4listobj <- c()
    dummy <- rep(0, length(set4sim_temp$time_s_diff))
    dummy_v <- c(set4sim_temp$speed1, dummy)
    dummy_s <- c(set4sim_temp$dist1, dummy)
    
    ## For each hypothesis
    for(j in 1:length(set4sim$computeI)) {
      
      ## Check if driver did not already passed corresponding objects
      ## Otherwise do not simulate driver behaviour
      if(j %in% c(1,3) | pos4carryout_m <= set4sim$objpos[j]) {

        ## For each speed model
        for(k in 1:length(set4sim$v_ms.max)) { 
          
          ## Get data from DVM for u
          ## For intention 1 and 2 this will be constant maximum u
          ## For intention 3 and 4 synthesised models will be used
          if (j %in% c(1, 2))
            u <- set4sim$v_ms.max[k] 
          else 
            u <- as.numeric(dat4dvm[dat4dvm == pos4carryout_m, paste("k", k, sep = "")])
          
          ## For each acceleration model
          for(l in 1:length(set4sim$acclon_ms2.max)) {

            ## Get acceleration
            acclon_ms2.max <- set4sim$acclon_ms2.max[l]
            a <- acclon_ms2.max * 0.01

            # v_sim.prev <- set4sim_temp$speed1
            # s_sim.prev <- set4sim_temp$dist1
            # lapply(set4sim_temp$time_s_diff, function(dt) {
            # ##When using individual timesteps
            #   #a <- acclon_ms2.max * dt
            #   if (j %in% c(1, 3)) {
            #     gap_des <- 0
            #     gap_act <- 1
            #   } else {
            #     gap_des <- idmGap_des(set4idm$d0, v_sim.prev, acclon_ms2.max, set4idm$b)
            #     gap_act <- idmGap_act(s_sim.prev, set4sim$objpos[j])
            #   }
            #   a_sim <- a * ( (1 - ( v_sim.prev / u)^set4idm$delta ) - ( gap_des / gap_act)^2 )
            #   v_sim.prev <<- v_sim.prev + a_sim
            #   s_sim.prev <<- s_sim.prev + v_sim.prev * dt
            # })
            # dat4sim2 <- append(dat4sim2,
            #                    list(data.frame(dist_m = s_sim.prev,
            #                                    speed_ms = v_sim.prev)))
            
            # Initialise object for simulation data
            v_sim.coll <- dummy_v
            s_sim.coll <- dummy_s
            i = 2
            # For each row in data (excluding first row)
            for(dt in set4sim_temp$time_s_diff) {

              ## When using individual timesteps
              #a <- acclon_ms2.max * dt

              v_sim.prev <- v_sim.coll[i-1]
              s_sim.prev <- s_sim.coll[i-1] 

              ## Compute current gap values
              if (j %in% c(1, 3)) {
                gap_des <- 0
                gap_act <- 1
              } else {
                gap_des <- idmGap_des(set4idm$d0, v_sim.prev, acclon_ms2.max, set4idm$b)
                gap_act <- idmGap_act(s_sim.prev, set4sim$objpos[j])
              }

              ## Compute new speed and distance
              a_sim <- a * ( (1 - ( v_sim.prev / u)^set4idm$delta ) - ( gap_des / gap_act)^2 )
              v_sim <- v_sim.prev + a_sim
              s_sim <- s_sim.prev + v_sim * dt

              v_sim.coll[i] <- v_sim
              s_sim.coll[i] <- s_sim

              # v_sim.prev <- v_sim.prev + a_sim
              # s_sim.prev <- s_sim.prev + v_sim.prev * dt

              i = i + 1
            } ## Data
            ## dat4sim2 <- append(dat4sim2, list(dat4sim_temp))
            dat4sim2 <- append(dat4sim2, list(data.frame(dist_m = s_sim.coll, speed_ms = v_sim.coll)))

             name4listobj <- 
               c(name4listobj,
                 paste(c("j", "k", "l"), c(j, k, l), collapse = "_", sep = ""))
             names(dat4sim2) <- name4listobj
            
          } ## Acceleration model
        } ## Speed model
        
      } #else { ## When driver already passed the object
        #outputString(paste("Driver already passed object in intention", j))
      #}
    } ## Intent
    
    names(dat4sim2) <- name4listobj
    
  return(dat4sim2)
}
     
    # ## For each hypothesis
    # for(j in 1:length(set4sim$computeI)) { 
      
      # ## For each speed model
      # for(k in 1:length(set4sim$v_ms.max)) { 
      #   
      #   ## Initialise u
      #   if (j %in% c(1, 2)) {
      #     dat4u <- rep(set4sim$v_ms.max[k], nrow(dat4sim))
      #     u <- dat4u[1]
      #   } else {
      #     dat4u <- 
      #       #dat4dvm.spread %>% 
      #       # dat4dvm.spread_v2 %>% 
      #       dat4dvm %>% 
      #       #filter(dist %in% dat4sim$sxx_dist_m_rnd1) %>% 
      #       filter(dist == set4sim$pos4carryout) %>% 
      #       data.frame()
      #     dat4u <- dat4u[, paste("k", k, sep = "")]
      #     dat4u <- as.numeric(dat4u)
      #     dat4u <- as.vector(dat4u)
      #     
      #     # ## Choose u as desired velocity at position of carry-out
      #     rowfinder <- which(dat4dvm.spread$dist == set4sim$pos4carryout)
      #     u <- dat4dvm.spread[rowfinder, paste("k", k, sep = "")]
      #     u <- as.numeric(u)
      #   }
      #   # print(u)

        ## Initialise data
        #dat4sim_temp <- dat4sim

    #     ## For each acceleration model
    #     for(l in 1:length(set4sim$acclon_ms2.max)) {
    #       
    #       ## Get acceleration
    #       acclon_ms2.max <- set4sim$acclon_ms2.max[l]
    #       
    #       ## Create variable names
    #       suffix <- paste(c("j", "k", "l"), c(j, k, l), collapse = "_", sep = "")
    #       suffix <- paste("_sim", suffix, sep = "_")
    #       varname4speed_sim <- paste(set4dat$varname4speed, suffix, sep = "")
    #       varname4dist_sim <- paste(set4dat$varname4dist_m, suffix, sep = "")
    # 
    #       ## For each row in data (excluding first row)
    #       for(i in 2:nrow(dat4sim)) { 
    #       
    #         objpos <- set4sim$objpos[j]
    #         ## Intention 1 and 3 will always be computed
    #         if(j %in% c(1,3) | set4sim$pos4carryout <= set4sim$objpos[j]) {
    #         
    #         
    #         ## Adjust parameters to each time step
    #         #time_s_diff <- dat4sim[i, set4dat$varname4time] - dat4sim[i-1, set4dat$varname4time]
    #         #b <- set4idm$b * time_s_diff
    #         a <- acclon_ms2.max * dat4sim$time_s_diff[i]
    # 
    #         v_sim.prev <- dat4sim[i-1, varname4speed_sim]
    #         s_sim.prev <- dat4sim[i-1, varname4dist_sim]
    #         
    #         ## Choose u as desired velocity from the current new position
    #         # u <- dat4u[i]
    #          # if(s_sim.prev <= 5)
    #          #   u <- 
    #          #  dat4dvm.spread %>% 
    #          #  filter(dist %in% round(s_sim.prev, 1))  else
    #          #    u <- 
    #          #  dat4dvm.spread %>% 
    #          #  filter(dist == 5)  
    #               
    #       
    # 
    #         ## Compute current gap values
    #         if (j %in% c(1, 3)) {
    #           gap_des <- 0
    #           gap_act <- 1
    #         } else {
    #           gap_des <- idmGap_des(set4idm$d0, v_sim.prev, acclon_ms2.max, set4idm$b)
    #           #gap_act <- idmGap_act(s_sim.prev, objpos)
    #           gap_act <- idmGap_act(s_sim.prev, objpos)
    #         }
    #         
    #         ## Compute new speed and distance
    #         a_sim <- a * ( (1 - ( v_sim.prev / u)^set4idm$delta ) - ( gap_des / gap_act)^2 )
    #         v_sim <- v_sim.prev + a_sim
    #         s_sim <- s_sim.prev + v_sim * dat4sim$time_s_diff[i]
    #       
    #         dat4sim[i, varname4speed_sim] <- v_sim
    #         dat4sim[i, varname4dist_sim] <- s_sim
    #         
    #         } else {
    #           dat4sim[i, varname4speed_sim] <- 0
    #           dat4sim[i, varname4dist_sim] <- objpos
    #         }
    #           
    #           
    #       } ## Row
    #       
    #       
    #       #dat4sim[, varname4speed_sim] <- dat4sim_temp[, set4dat$varname4speed]
    #       #dat4sim[, varname4dist_sim] <- dat4sim_temp[, set4dat$varname4dist_m]
    #       
    #     } ## Acc
    #   } ## Speed
    # } ## Intent
#   } ## Simulation-based approach
# 
#   ## else comparison based TBD
#   #return(dat4sim)
#   return(dat4sim2)
# }




# ALT ---------------------------------------------------------------------
# 
# dat4sim <- dat4sim
# dat4sim <- left_join(dat4sim,
#                      dat4dvm.spread %>% filter(dist %in% dat4sim$sxx_dist_m_rnd1),
#                      by = setNames("dist", set4dat$varname4dist_m))
# 
# plotdat <-
#   ggplot() +
#   geom_line(data = dat4sim, aes(x = sxx_dist_m_rnd1, y = speed_ms))
# 
# for(j in 1:4) {
#   for(k in 1:3) {
#     for(l in 1:3) {
# 
#       dat4sim_temp <- dat4sim
#       dat4sim_temp$dist2 <- 0
#       acclon_ms2.max <- set4sim$acclon_ms2.max[l]
# 
#       for(i in 2:nrow(dat4sim)) {
# 
#         s_prev <- dat4sim[i-1, "dist2"]
#         u <- predLiebner_getu(j, k, dat4dvm.spread, s_prev)
#         v_prev <- dat4sim_temp[i-1, set4dat$varname4speed]
# 
#         time_s_diff <- dat4sim[i, set4dat$varname4time] - dat4sim[i-1, set4dat$varname4time]
#         b <- set4idm$b * time_s_diff
#         a <- acclon_ms2.max * time_s_diff
# 
#         if (j %in% c(1, 3)) {
#           gap_des <- 0
#           gap_act <- 1
#         } else {
#           gap_des <- idmGap_desired(set4idm$d0, v_prev, a, b)#acclon_ms2.max,set4idm$b)
#           gap_act <- idmGap_actual(s_prev, objpos)
#         }
# 
#         a_sim <- idmAcc(a, v_prev, u, set4idm$delta, gap_des, gap_act)
#         v_sim <- v_prev + a_sim
#         s_sim <- s_prev + v_sim * time_s_diff
# 
#         dat4sim_temp[i, set4dat$varname4speed] <- v_sim
#         dat4sim_temp[i, "dist2"] <- s_sim
#       }
# 
#       if (j == 1) c = "blue"
#       if (j == 2) c = "orange"
#       if (j == 3) c = "red"
#       if (j == 4) c = "magenta"
# 
#       plotdat <-
#         plotdat +
#         geom_line(data = dat4sim_temp, aes(x = sxx_dist_m_rnd1, y = speed_ms), col = c)
# 
#     }
#   }
# }
# 
# 
