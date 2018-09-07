predLiebner_modelDrivBehav_batch <- function(states_n_I,
                                             states_n_S,
                                             states_n_A,
                                             obj_positions,
                                             thresholds_u_max,
                                             dat_dsm_spread,
                                             am2,
                                             thresholds_acc_lon_max,
                                             speed1,
                                             idm_delta,
                                             am1,
                                             idm_d0,
                                             idm_b,
                                             time_s_diff) {

  coll_sim <- c()
  u_coll <- c()
  
  ## For each hypothesis
  for(j in 1:states_n_I) {
    
    ## Get object position corresponding to intention
    obj_position <- obj_positions[j]
    
      ## For each speed model
      for(k in 1:states_n_S) {
        
        ## Get data from dsm for u
        ## For intention 1 and 2 this will be constant maximum u
        ## For intention 3 and 4 synthesized models will be used
        if (j %in% c(1, 2)) {
          u <- thresholds_u_max[k]
        } else {
          row_finder <- dat_dsm_spread$am == am2
          col_finder <- paste0("k", k)
          u <- dat_dsm_spread[row_finder, col_finder]
        }
        
        ## For each acceleration model
        for(l in 1:states_n_A) {
        
          ## Get acceleration
          acc_lon_ms2_max <- thresholds_acc_lon_max[l]

            temp <- sim_asv(speed1, 
                            u, 
                            idm_delta, 
                            am1, 
                            idm_d0, 
                            acc_lon_ms2_max, 
                            idm_b, 
                            obj_position, 
                            j,
                            am2,
                            time_s_diff)
            component_finder <- paste0(c("Ij", "Mk", "al"), c(j, k, l), collapse = "_")
            coll_sim[[component_finder]] <- temp

        } ## Acceleration model
      } ## Speed model
  } ## Intent
  
  return(coll_sim)
}

