## Likelihood of current observation
## ... given the longitudinal behavior of the driver could be modeled
## ... by the parameter set associated with hypothesis Hi

predLiebner_compProb_O_Hi <- function(set4sim, pos4carryout_m, dat4sim, P_O_Hi) {

    ## Get actual values
    dist_m.act <- set4sim_temp$dist2
    speed_ms.act <- set4sim_temp$speed1
    #print(speed_ms.act)
    
    ## Get final values from IDM simulation
    # obs <- lapply(dat4sim, function(x) {
    #   if (is.null(x))
    #     x <- data.frame(dist_m = NA, speed_ms = NA) else
    #       x <- x
    #   })
    # #obs <- dat4sim
    # obs <- lapply(obs, function(x) tail(x, 1))
    obs <- lapply(dat4sim, function(x) 
              lapply(x, function(y) 
                #if (is.null(y)) NA else
                tail(y, 1)) )
    obs <- as.data.frame(data.table::rbindlist(obs, idcol = T, fill  = T))
    #obs <- dat4sim[, lapply(.SD, last), hyp]
    
    ## Compute standard deviations for simulated distances and speed
    sigma_s_m <- sd(obs$dist_m)
    sigma_v_ms <- sd(obs$speed_ms)
    #coll4sd <<- data.table::rbindlist(list(coll4sd, data.frame(sigma_s_m, sigma_v_ms)))
    #sigma_s_m <- 1.2
    #sigma_v_ms <- 1.2
    #sigma_s_m <- 1
    #sigma_v_ms <- 1
    #sigma_s_m <- 0.6
    # sigma_v_ms <- 0.6
    # sigma_s_m <- 1.8
    # sigma_v_ms <- 1.8
    # sigma_s_m <- 1.5 * 0.8
    # sigma_v_ms <- 2 * 0.8
    # sigma_s_m <- 1.5
    # sigma_v_ms <- 2
    #sigma_s_m <- 1.2
    #sigma_v_ms <- 1.7

    for(i in 1:length(obs$.id)) {
    #for(i in 1:length(obs$hyp)) {

      ## Create name for current likelihood corresponding to:
      ## Hypothesis i _ Intent j _ Velocity model k _ Acceleration model l
      #suffix_temp <- names(dat4sim)[i]
      #coll4names <- c(coll4names, suffix_temp)
      
      #print(obs$dist_m[i])
      
      if (is.na(obs$dist_m[i]))
        y = 0 else
      
      y <- predLiebner_pdf4sim(dist_m.act,
                               obs$dist_m[i],
                               sigma_s_m,
                               speed_ms.act,
                               obs$speed_ms[i],
                               sigma_v_ms)
      
      ## Collect probability of current observation
      P_O_Hi[, obs$.id[i]] <- y
      #P_O_Hi[, obs$hyp[i]] <- y

    }
    return(P_O_Hi)
}


      
    
    # colnames4dist <-
    #   colnames(dat4prob$obs)[grepl(paste(set4dat$varname4dist_m, "sim", sep = "_"), cols)]
    # colnames4speed <-
    #   colnames(dat4prob$obs)[grepl(paste(set4dat$varname4speed, "sim", sep = "_"), cols)]

    # dat4prob$sigma_s_m <- sd(dat4prob$obs[, colnames4dist], na.rm = T)
    # dat4prob$sigma_v_ms <- sd(dat4prob$obs[, colnames4speed], na.rm = T)
    #print(dat4prob$sigma_s_m)
    #print(dat4prob$sigma_v_ms)
    
    #for(j in 1:length(set4sim$computeI)) { ## For each intent ...
      
      #if(j %in% c(2,4) | set4sim$pos4carryout <= set4sim$objpos[j])
      ## New column names
      # colnames4dist <-
      #   colnames(dat4prob$obs)[grepl(paste(set4dat$varname4dist_m, "sim", paste("j", j, sep = ""), sep = "_"), cols)]
      # colnames4speed <-
      #   colnames(dat4prob$obs)[grepl(paste(set4dat$varname4speed, "sim", paste("j", j, sep = ""), sep = "_"), cols)]
      # 
      # dat4prob$sigma_s_m <- sd(dat4prob$obs[, colnames4dist], na.rm = T)
      # dat4prob$sigma_v_ms <- sd(dat4prob$obs[, colnames4speed], na.rm = T)
      # # }
      
      #for(k in 1:length(set4sim$v_ms.max)) { ## For desired velocity model ...
        #for(l in 1:length(set4sim$acclon_ms2.max)) { ## For each acc model ...
          
          # if(j %in% c(1,3) | set4sim$pos4carryout <= set4sim$objpos[j]) {
          #i = i + 1 ## Increase run counter
          
          ## Define current hypothesis index
          #suffix_temp <- paste("j", j, "_k", k, "_l", l, sep = "")
          
          # if(j %in% c(2,4) & set4sim$pos4carryout <= set4sim$objpos[j]) {
          #   y = 0
          # } else 
          # {
          #   ## Each velocity profile of the hyotheses terminate at ...
          #   ## ... different distances and velocities
          #   dat4prob$dist_m.est <-
          #     dat4prob$obs[, paste(set4dat$varname4dist_m, "sim", suffix_temp, sep = "_")]
          #   dat4prob$speed_ms.est <-
          #     dat4prob$obs[, paste(set4dat$varname4speed, "sim", suffix_temp, sep = "_")]
            
            
            # colnames4dist <-
            #   colnames(dat4prob$obs)[grepl(paste(set4dat$varname4dist_m, "sim", paste("j", j, sep = ""), paste("k", k, sep = ""), sep = "_"), cols)]
            # colnames4speed <-
            #   colnames(dat4prob$obs)[grepl(paste(set4dat$varname4speed, "sim", paste("j", j, sep = ""), paste("k", k, sep = ""), sep = "_"), cols)]
            # 
            # dat4prob$sigma_s_m <- sd(dat4prob$obs[, colnames4dist], na.rm = T)
            # dat4prob$sigma_v_ms <- sd(dat4prob$obs[, colnames4speed], na.rm = T)
            
            ## Empirical parameters (from papers) for pdf
            # dat4prob$sigma_s_m <- 1.2
            # dat4prob$sigma_v_ms <- 1.2
            
            ## Compute probability of current observation
          #   y <- predLiebner_pdf4sim(dat4prob$dist_m.act,
          #                            dat4prob$dist_m.est,
          #                            dat4prob$sigma_s_m,
          #                            dat4prob$speed_ms.act,
          #                            dat4prob$speed_ms.est,
          #                            dat4prob$sigma_v_ms)
          # }
          # 
         
            
            
          # } else {
          # 
          #   y <- 0
          # 
          # }
          
          # ## Collect probability of current observation
          # dat4prob$P_O_Hi <- cbind(dat4prob$P_O_Hi, y)
          # 
          # ## Create name for current likelihood corresponding to:
          # ## Hypothesis i _ Intent j _ Velocity model k _ Acceleration model l
          # coll4names <- c(coll4names, paste("P_O_Hi", i, suffix_temp, sep ="_"))
          
    #     } ## Acceleration model
    #   } ## Desired velocity model
    # } ## Intent
    
    ## Rename columns according to hypothesis indices
    # colnames(dat4prob$P_O_Hi) <- coll4names
    
  # } ## Simulation-based approach
  # 
  # 
  # 
  # if(set4algo$hypscore == "comparison-based") {
  #   
  #   ## Get complete values from IDM simulation
  #   dat4prob$obs <- dat4sim
  #   
  #   ## Complete list for columns related to IDM accelerations
  #   cols4acclon <- colnames(dat4prob$obs)[grepl(paste(set4dat$varname4acclon, "sim", sep = "_"), cols)]
  #   
  #   ## Initialise collector variable for probabilies over time
  #   coll <- c()
  #   
  #   for(row in 2:nrow(dat4sim)) { ## For each data row
  #     
  #     if(set4algo$hypscore == "comparison-based") {
  #       dat4prob$sigma_a_ms2 <- sd(dat4prob$obs[row, cols4acclon], na.rm = T)
  #     }
  #     
  #     ## Compute values at each observed time step
  #     dat4prob$acc_ms2.act <- dat4prob$obs$acclon_ms2[row]
  #     
  #     ## Likelihood of current observation
  #     ## ... given the longitudinal behavior of the driver could be modeled
  #     ## ... by the parameter set associated with hypothesis Hi
  #     ## Initialise collector
  #     dat4prob$P_O_Hi_temp <- c() ## Gets 36 values
  #     
  #     ## Initialise helper for collnames
  #     coll4names <- c()
  #     
  #     ## Initialise counter
  #     i <- 0
  #     
  #     for(j in 1:length(set4sim$computeI)) { # For each intent ...
  #       for(k in 1:length(set4sim$v_ms.max)) { # For each v model ...
  #         for(l in 1:length(set4sim$acclon_ms2.max)) { #For each a model ...
  #           
  #           i = i+1  # Increase counter
  #           
  #           ## Set index
  #           suffix_temp <- paste("j", j, "_k", k, "_l", l, sep = "")
  #           
  #           dat4prob$acclon_ms2.est <-
  #             dat4prob$obs[row, paste(set4dat$varname4acclon, "sim", suffix_temp, sep = "_")] #/ dat4sim$time_s_diff[row]
  #           
  #           y <- predLiebner_pdf4comp(dat4prob$acc_ms2.act,
  #                                     dat4prob$acclon_ms2.est,
  #                                     dat4prob$sigma_a_ms2)
  #           
  #           ## In case of already passed objects
  #           if(j == 2 & dat4sim$dist2sx_m_v2a_rnd1[row] >= set4sim$objpos[2])
  #             y <- 0
  #           
  #           if(j == 4 & dat4sim$dist2sx_m_v2a_rnd1[row] >= set4sim$objpos[4])
  #             y <- 0
  #           
  #           ## Collect likelihood of current observation
  #           dat4prob$P_O_Hi_temp <- cbind(dat4prob$P_O_Hi_temp, y)
  #           
  #           ## Create and collect name for current likelihood corresponding to:
  #           ## Hypothesis i, Intent j, Velocity model k, Acceleration model l
  #           coll4names <- c(coll4names, paste("P_O_Hi", i, suffix_temp, sep =""))
  #           
  #         } # a model
  #       } ## v model
  #     } ## Intent
  #     
  #     ## Rename columns
  #     colnames(dat4prob$P_O_Hi_temp) <- coll4names
  #     
  #     ## Collect probabilites for P(O|Hi) in observed time
  #     coll <- rbind(coll, dat4prob$P_O_Hi_temp)
  #     
  #   }
  #   
  #   ## Compute means over observed time
  #   dat4prob$P_O_Hi <- colMeans(coll)

#}