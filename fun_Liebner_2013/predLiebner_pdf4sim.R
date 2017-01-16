predLiebner_pdf4sim <- function (s, s.est, s.sd, v, v.est, v.sd) {
  e = ( (s - s.est) / s.sd )^2 + ( (v - v.est) / v.sd )^2
  e = sqrt(e)
  y = ( 1 / ( 2 * pi * s.sd * v.sd) ) * exp( -1/2 * e^2 )
  return(y)
}

# 
# dat4prob$dist_m.act
# s_est
# dat4prob$sigma_s_m
# dat4prob$speed_ms.act
# v_est
# dat4prob$sigma_v_ms
# 
# e_test <- sqrt( ( (dat4prob$dist_m.act - s_est$sim_dist_val) / dat4prob$sigma_s_m) ^2 + ( (dat4prob$speed_ms.act - v_est$sim_speed_val) / dat4prob$sigma_v_ms) ^2 )
# y_test <- 1 / ( 2 * pi * dat4prob$sigma_s_m * dat4prob$sigma_v_ms) * exp (-1/2 * e_test^2)
