source("analysis-study-1/prediction_dev/zz-vis-classic-template_single.R")

## Compute starting values for simulation
#pos4carryout <- round(pos4carryout, 1)
#pos4carryout <- sett_sim$pos4carryout
sett_sim_temp <- predLiebner_getStartVal4Sim(dat_test, sett_dat, sett_proc$carryout_am1)

if (sett_sim_temp$time1 < (min(dat_test$time_s) - sett_sim$timelag_s)) {
  
  ## If no date before timelag is available: Skip prediction
  messageWithSepLine(paste(paste("* Simulation at:", s, "failed"), 
                           "(No data available before time lag",
                           sep = "\n"))
  s <- s + sett_proc$carryout_step
  opt <- options(show.error.messages=FALSE) 
  on.exit(options(opt)) 
  stop()
}

sett_sim_temp$time_s_diff <- rep(0.01, 1 / 0.01)
sett_sim_temp$pos4carryout_precise <- sett_sim_temp$dist2


coll4simtail <- coll4simtail_template
## 
# sett_sim$objpos[1] <- 9999
# sett_sim$objpos[3] <- 9999
ptm3 <- proc.time()
source("analysis-study-1/prediction_dev/xx-pred.R")
#outputProcTime(ptm3)
results4output <- data.frame(P_O_Hi = results)
#print(round(results4output * 100, 2))

## Visualisation
#source("analysis-study-1/prediction_dev/xz-vis.R")
coll4dat_sim <- c()
coll4results <- c()
source("analysis-study-1/prediction_dev/xz-vis-classic_single.R")
