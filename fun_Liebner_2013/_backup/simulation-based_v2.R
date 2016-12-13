for(i in 2:nrow(dat4sim)) { # For each row in data (excluding first row)
  time_s_diff <- dat4sim$time_s_diff[i]
  b <- set4idm$b * time_s_diff
  for(j in 1:length(set4sim$computeI)) {
    if (j %in% c(1,3) | dat4sim[i, set4dat$varname4dist_m] <= set4sim$objpos[j]) {
      objpos <- set4sim$objpos[j]
      for(k in 1:length(set4sim$v_ms.max)) { # For each velocity model
        # Get desired velocity (fixed or modelled, depending on intent)
        if (j %in% c(1, 2)) u <- set4sim$v_ms.max[k] else
          u <- dat4sim[i-1, paste("k", k, sep = "")]
        for(l in 1:length(set4sim$acclon_ms2.max)) { # For each acceleration model
          Variable identification suffix for model combination
          suffix <- paste(c("j", "k", "l"), c(j, k, l), collapse = "_", sep = "")
          suffix <- paste("_sim", suffix, sep = "_")
          
          varname4speed_sim <- paste(set4dat$varname4speed, suffix, sep = "")
          varname4dist_sim <- paste(set4dat$varname4dist_m, suffix, sep = "")
          
          ## Get acceleration
          acclon_ms2.max <- set4sim$acclon_ms2.max[l]
          a <- acclon_ms2.max * time_s_diff
          
          ## Previous simulation values
          v_sim.prev <- dat4sim[i-1, varname4speed_sim]
          s_sim.prev <- dat4sim[i-1, varname4dist_sim]
          
          ## Compute current gap values
          temp_gap <- idmGap(j, set4idm$d0, v_sim.prev, acclon_ms2.max, set4idm$b, s_sim.prev, objpos)
          gap_desired <- temp_gap$gap_desired
          gap_actual <- temp_gap$gap_actual
          
          ## Simulate new velocity and distance
          dat4sim[i, varname4speed_sim] <- idmSpeed(v_sim.prev, a, u, set4idm$delta, gap_desired, gap_actual)
          dat4sim[i, varname4dist_sim] <- idmDistance(s_sim.prev, dat4sim[i, varname4speed_sim], time_s_diff)
        }
      }
    }
  }
}