predLiebner_pdf4sim <- function (s, 
                                 s_est, 
                                 s_sd, 
                                 v, 
                                 v_est, 
                                 v_sd) {
  
  e <- ( (s - s_est) / s_sd )^2 + ( (v - v_est) / v_sd )^2
  e <- sqrt(e)
  y <- ( 1 / ( 2 * pi * s_sd * v_sd) ) * exp( -1/2 * e^2 )
  
  return(y)
}