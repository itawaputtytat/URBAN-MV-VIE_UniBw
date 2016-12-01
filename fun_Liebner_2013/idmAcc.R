idmAcc <- function(a, v_sim.prev, u, delta, d0.des, d0.act) {
  ## a as maximum longitudinal acceleration (model l; m/s^2)
  ## v as previous speed value (m/s)
  ## u as desired velocity (fixed or modelled value; depending on intent; km/h)
  ## delta as acceleration exponent
  ## d0.des as desired minimum gap to leading vehicle (m)
  ## d0.act as actual minimum gap to leading vehicle (m)
  
  IDM_acc <-  a * ( (1 - ( v_sim.prev / u)^delta ) - ( d0.des / d0.act)^2 )
  #  dat4sim[i, paste("acclon_ms2", suffix, sep = "")] <<- IDM_acc
  
  return(IDM_acc)
}
