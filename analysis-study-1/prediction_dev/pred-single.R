ptm <- proc.time()
## Initialise temporary simulation settings
sett_sim_temp <- c()
sett_sim_temp$dist2 <- sett_proc$carryout_am_single
sett_sim_temp$dist2_prev <- NA
sett_sim_temp$am_1st <- NA

## Compute starting values for simulation
#pos4carryout <- round(pos4carryout, 1)
#pos4carryout <- sett_sim$pos4carryout
sett_sim_temp <- 
  modifyList(sett_sim_temp,
             predLiebner_getStartVal4Sim(dat_test, 
                                         sett_dat, 
                                         sett_sim_temp))

if (sett_sim_temp$time1 < (min(dat_test$time_s) - sett_sim$timelag_s)) {
  
  ## If no date before timelag is available: Skip prediction
  messageWithSepLine(c(paste("* Simulation at:", sett_sim_temp$dist2, "failed"), 
                       "(No data available before time lag"))
  opt <- options(show.error.messages=FALSE) 
  on.exit(options(opt)) 
  stop()
}

source("analysis-study-1/prediction_dev/pred.R")
#results4output <- data.frame(P_O_Hi = dat_pred_results)

source("analysis-study-1/prediction_dev/_plot-template_single.R")
source("analysis-study-1/prediction_dev/_plot_single.R")

outputProcTime(ptm)