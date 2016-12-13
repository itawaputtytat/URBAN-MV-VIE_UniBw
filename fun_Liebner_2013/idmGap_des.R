idmGap_des <- function(d0, v, a, b) {
  ## d0 as minimum gap to leading vehicle (m)
  ## v as previous simulated velocity  (m/s^2)
  ## a as maximum longitudinal acceleration (model l; m/s^2)
  ## b as comfortable deceleration (m/s^2)
  gap_des <- d0 + d0 * v + ( v^2 / ( 2 * (a * b)^(1/2) ) )
  return(gap_des)
}


