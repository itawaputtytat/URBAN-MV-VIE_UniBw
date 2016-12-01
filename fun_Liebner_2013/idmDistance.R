idmDistance <- function(s_sim.prev, v_sim, t_diff) {
  ## s_sim.prev as previous simulated speed distance (m)
  ## v_sim as current simulated velocity  (m/s^2)
  ## t_diff as time difference between data steps (s)
  
  # Compute new distance values
  # = last distance value + new speed * time difference
  IDM_distance <- s_sim.prev + v_sim * t_diff
  
  return(IDM_distance)
  
}
