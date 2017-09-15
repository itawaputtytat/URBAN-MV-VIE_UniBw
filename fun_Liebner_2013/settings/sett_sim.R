sett_sim <- c()
sett_sim$timelag_s <- -1       ## Time lag (data basis for simulation) (s)
#sett_sim$v_ms.max <- c(13, 15, 18)      ## Maximum desired velocity (ms)
sett_sim$v_ms.max <- c(48/3.6, 54/3.6, 60/3.6)
sett_sim$acc_lon_ms2.max <- c(1.5, 2, 2.5)  ## Maximum acceleration  (m/s^2)
sett_sim$objpos <- c(0, -9, 0, 7)   ## Position for virtual obstacle
sett_sim$cutcurves <- T              ## Complete curves family vs. cut
sett_sim$cutcurves_time_s <- 1       ## Duration of curves family
sett_sim$computeI <- c(T, T, T, T)   ## Presence of intention
## Intention 4: unlike paper!!!???



## Initialise collector for simulation tails
## Compute number of hypothesis
hypothesis_n <-
  c(j = length(sett_sim$computeI),
    k = length(sett_sim$v_ms.max),
    l = length(sett_sim$acc_lon_ms2.max))
hypothesis_n_overall <- prod(hypothesis_n)
## Initialise list with a component for each hypothesis
coll4simtail_template <- vector("list", hypothesis_n_overall)
## Rename list components
names(coll4simtail_template) <-
  names(idm_createSimDat(hypothesis_n, varnames = "", prefix = ""))