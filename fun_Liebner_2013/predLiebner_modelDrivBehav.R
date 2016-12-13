predLiebner_modelDrivBehav <- function(j, 
                                     set4idm, 
                                     v_sim.prev,
                                     a_max,
                                     objpos,
                                     s_sim.prev,
                                     time_s_diff, 
                                     u) {

  ## Compute current gap values
  if (j %in% c(1, 3)) {
    gap_desired <- 0
    gap_actual <- 1
  } else {
    gap_desired <- idmGap_desired(set4idm$d0, v_sim.prev, a_max, b)#acclon_ms2.max,set4idm$b)
    gap_actual <- idmGap_actual(s_sim.prev, objpos)
  }
  
  ## Compute new speed and distance
  sim_speed <- idmSpeed(v_sim.prev, a_max, #acclon_ms2.max, 
                        u, set4idm$delta, gap_desired, gap_actual)
  sim_dist <- idmDistance(s_sim.prev, sim_speed, time_s_diff)
  
  return(list(sim_speed = sim_speed, 
              sim_dist = sim_dist))
}