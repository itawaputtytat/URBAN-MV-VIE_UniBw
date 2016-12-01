idmSim <-
  #function(dat4sim, computeI, model_v.max, model_acclon.max, idm_b, idm_pos4obj, d0, delta) {
  function(suffix, model_acclon.max, time_s_diff, d0,
           idm_b, idm_pos4obj,
           u, delta) {
    
    # Variable identification suffix for model combination
    a <- model_acclon.max * time_s_diff
    
    ## Previous simulated values
    v_sim.prev <- dat4sim[i-1, paste("speed_ms", suffix, sep = "")]
    s_sim.prev <- dat4sim[i-1, paste("dist2sx_m_v2a_rnd1", suffix, sep ="")]
    
    ## Compute current gap values
    temp_gap <- computeIDM_gap(j, d0, v_sim.prev, model_acclon.max, idm_b, s_sim.prev, idm_pos4obj)
    gap_desired <- temp_gap$gap_desired
    gap_actual <- temp_gap$gap_actual
    
    ## Compute new simulated velocity value
    dat4sim[i, paste("speed_ms", suffix, sep = "")] <<-
      computeIDM_speed(v_sim.prev, a, u, delta, gap_desired, gap_actual)
    
    ## Compute new distance value
    dat4sim[i, paste("dist2sx_m_v2a_rnd1", suffix, sep = "")] <<-
      computeIDM_distance(s_sim.prev,
                          dat4sim[i, paste("speed_ms", suffix, sep = "")],
                          time_s_diff)
    return(dat4sim)
  }

