#computeIDM_speed <- function(v_sim.prev, a_sim) {
idmSpeed <- function(v_sim.prev, a, u, delta, gap_desired, gap_actual) {
  ## v_sim.prev as previous simulated speed values (m/s)
  ## a_sim as simulated acceleration (m/s^2)
  
  # compute new speed values
  # = last speed + acceleration (time difference already included)
  a_sim <- computeIDM_acc(a, v_sim.prev, u, delta, gap_desired, gap_actual)
  IDM_speed <- v_sim.prev + a_sim
  
  return(IDM_speed)
}
