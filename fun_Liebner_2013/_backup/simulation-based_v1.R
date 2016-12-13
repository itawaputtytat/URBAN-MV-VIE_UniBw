for(i in 2:nrow(dat4sim)) { # For each row in data (excluding first row)
  time_s_diff <- dat4sim$time_s_diff[i]
  b <- set4idm$b * time_s_diff

  for(j in 1:length(set4sim$computeI)) {

    objpos <- set4sim$objpos[j]

    if(j %in% c(1,3) | dat4sim[i, set4dat$varname4dist_m] <= set4sim$objpos[j]) {

      for(k in 1:length(set4sim$v_ms.max)) { # For each velocity model

        ## Get desired velocity (fixed or modelled, depending on intent)
        if(j %in% c(1, 2)) u <- set4sim$v_ms.max[k] else
          u <- dat4sim[i-1, paste("k", k, sep = "")]

        for(l in 1:length(set4sim$acclon_ms2.max)) { # For each acceleration model

          # Variable identification suffix for model combination
          suffix <- paste("_sim_j", j, "_k", k, "_l", l, sep = "")

          ## Get acceleration
          acclon_ms2.max <- set4sim$acclon_ms2.max[l]
          a <- acclon_ms2.max * time_s_diff

          ## Previous simulated values
          v_sim.prev <- dat4sim[i-1, paste(set4dat$varname4speed, suffix, sep = "")]
          s_sim.prev <- dat4sim[i-1, paste(set4dat$varname4dist_m, suffix, sep ="")]

          ## Compute current gap values
          temp_gap <- idmGap(j, set4idm$d0, v_sim.prev, acclon_ms2.max, set4idm$b, s_sim.prev, objpos)
          gap_desired <- temp_gap$gap_desired
          gap_actual <- temp_gap$gap_actual

          ## Compute new simulated velocity value
          dat4sim[i, paste(set4dat$varname4speed, suffix, sep = "")] <-
            idmSpeed(v_sim.prev, a, u, set4idm$delta, gap_desired, gap_actual)

          ## Compute new distance value
          dat4sim[i, paste(set4dat$varname4dist_m, suffix, sep = "")] <-
            idmDistance(s_sim.prev,
                                dat4sim[i, paste(set4dat$varname4speed, suffix, sep = "")],
                                time_s_diff)
        } # End of acceleration model
      } # End of velocity model
    } # End of if
  } ## End of intent
  #printProgress(i, nrow(dat4sim))
} # i in data


# #test <- c()
# if(set4sim$cutcurves == T)
#   dat4sim <- dat4sim %>% filter(time_s <= min(time_s) + (set4sim$cutcurves_time_s))
#   #dat4sim <- dat4sim %>% filter(time_s <= min(time_s) + (set4sim$cutcurves_time_s*-1))