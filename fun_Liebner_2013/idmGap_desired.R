idmGap_desired <- function(d0, v_sim.prev, a, b) {
  ## d0 as minimum gap to leading vehicle (m)
  ## v_sim.prev as oprevious simulated velocity  (m/s^2)
  ## a as maximum longitudinal acceleration (model l; m/s^2)
  ## b as comfortable deceleration (m/s^2)
  
  IDM_gap.desired <- d0 + d0 * v_sim.prev + ( v_sim.prev^2 / ( 2 * (a * b)^(1/2) ) )
  
  return(IDM_gap.desired)
}


