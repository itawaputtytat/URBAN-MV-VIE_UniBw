set4sim <- c()
set4sim$pos4carryout <- -17     ## Position where simulation is carried out (m)
set4sim$timelag_s <- -1       ## Time lag (data basis for simulation) (s)
#set4sim$v_ms.max <- c(13, 15, 18)      ## Maximum desired velocity (ms)
set4sim$v_ms.max <- c(48/3.6, 54/3.6, 60/3.6)
set4sim$acclon_ms2.max <- c(1.5, 2, 2.5)  ## Maximum acceleration  (m/s^2)
set4sim$objpos <- c(0, -9, 0, 7)   ## Position for virtual obstacle
set4sim$cutcurves <- T              ## Complete curves family vs. cut
set4sim$cutcurves_time_s <- 1       ## Duration of curves family
set4sim$computeI <- c(T, T, T, T)   ## Presence of intention
## Intention 4: unlike paper!!!???