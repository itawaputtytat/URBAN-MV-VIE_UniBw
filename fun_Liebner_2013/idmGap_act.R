idmGap_act <- function(s, objpos) {
  ## s_sim.prev as oprevious simulated distance (s)
  ## objpos object postion
  gap_act <- s - objpos
  return(gap_act)
}

