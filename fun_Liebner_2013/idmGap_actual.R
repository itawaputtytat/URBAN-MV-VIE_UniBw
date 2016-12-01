idmGap_actual <- function(s_sim.prev, objpos) {
  ## s_sim.prev as oprevious simulated distance (s)
  ## objpos object postion
  
  IDM_gap.actual <- s_sim.prev - objpos
  
  return(IDM_gap.actual)
}

