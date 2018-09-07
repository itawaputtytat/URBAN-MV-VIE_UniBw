
# Settings for simulation -------------------------------------------------

## time_lag_s:        Time lag (data basis for simulation) (s)
## obj_pos:           Position for virtual obstacle
## cut_curves:        Complete curves family vs. cut
## cutcurves_time_s:  Duration of curves family
## compute_intention: Presence of intention

sett_sim <- c()
sett_sim$time_lag_s <- -1       
sett_sim$obj_pos <- c(0, -9, 0, 7)   
sett_sim$cut_curves <- T              
sett_sim$cut_curves_time_s <- 1       
sett_sim$compute_I <- c(T, T, T, T)
## Intention 4: unlike paper!!!???