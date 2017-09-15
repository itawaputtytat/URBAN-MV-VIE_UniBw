am_1st <- NA

if (sett_proc$plot)
  source("analysis-study-1/prediction_dev/zz-vis-classic-template_complete.R")

#dist2start <- -35
#dist2end <- 20
#distint <- 1 # 0.1 # 0.5

## Initialise collector for results
#coll4results <- c()
results <- sett_bn$prior$I
names(results) <- sett_bn$states$I
coll4results <- data.frame(pos4carryout = sett_proc$carryout_am1, t(results))

dummy <-
  data.frame(dist_m = sett_proc$carryout_am1,
             speed_ms = dat_test$speed_ms[which(dat_test[, sett_dat$col_name_am] == -40)])
dummy_names <- names(idm_createSimDat(c(j = 4, k = 3, l = 3), varnames = "", prefix = ""))
coll4dat_sim <- vector("list", length(dummy_names))
names(coll4dat_sim) <- dummy_names
coll4dat_sim <- lapply(coll4dat_sim, function(x)
  x <- dummy)
#coll4prior <- c()

## Loop through passing
ptm2 <- proc.time()


# test <- lapply(dat_sim, function(x) lapply(x, function(y){
#   tail(y, 1)
#   print(y)
# } ))

coll4simtail <- coll4simtail_template

## Real-time
#s = 151; while (s < 721) {
#for(s in seq(-40,20,0.1)) {
#for(s in seq(-40,20,0.5)) {
#for(s in seq(sett_proc$carryout_am1,sett_proc$carryout_am2,sett_proc$carryout_step)) {
s <- sett_proc$carryout_am1

dist2_prev <- NA

while (s <= sett_proc$carryout_am2) { 


  
  ## Find actual starting position for simulation
  sett_sim_temp <- predLiebner_getStartVal4Sim(dat_test, sett_dat, s)
   if (sett_sim_temp$time1 < (min(dat_test$time_s) - sett_sim$timelag_s)) {

    ## If no date before timelag is available: Skip prediction
    messageWithSepLine(paste(paste("* Simulation at:", s, "failed"), 
                             "(No data available before time lag",
                             sep = "\n"))
    s <- s + sett_proc$carryout_step
    next
  }

  if (is.na(am_1st))
    am_1st <- sett_sim_temp$dist2
  
  sett_sim_temp$pos4carryout_precise <- sett_sim_temp$dist2
  #print(sett_sim_temp$dist1)
  #print(time_s_diff)
  #time_s_diff <- sett_sim_temp$time_s_diff
  sett_sim_temp$time_s_diff <- rep(0.01, 1 / 0.01)
  source("analysis-study-1/prediction_dev/xx-pred.R")
  
  ## Collect results
  # coll4results <- 
  #   data.table::rbindlist(list(coll4results, 
  #                              data.frame(s, t(results))))
  coll4results <-
    data.table::rbindlist(list(coll4results,
                               data.frame(sett_sim_temp$pos4carryout_precise, t(results))))
  
  temp2 <- lapply(dat_sim, function(x) 
    lapply(x, function(y) 
      if (is.null(y)) NA else
        tail(y, 1)) )
  temp2 <- data.table::rbindlist(temp2, idcol = T, fill  = T)
  
  
  # Collect tails of simulates speed profiles (history / trace)
  temp <- lapply(dat_sim, function(x)
    lapply(x, function(y)
      tail(y, 1)) )
  temp <- as.data.frame(data.table::rbindlist(temp, idcol = T, fill  = T))
  for(i in seq_along(temp$.id)) {
    finder <- temp$.id[i]
    coll4dat_sim[[finder]] <- rbind(coll4dat_sim[[finder]], temp[i,2:3])
  }
  
  if (sett_proc$plot) {
    if (sett_proc$stepwise_pause) {
      #pauseAndContinue()
      input <- 0
      while(T) {
          outputString(paste("[1] >>>",
                             "[2] <<<",
                             "[3] x ",
                             sep = "\n"))
          input <- readline(">>>")
        if (input == 1) 
          s <- s + 1 * sett_proc$carryout_step
        if (input == 2) 
          s <- s - 1 * sett_proc$carryout_step
        if (input == 3) 
          break   
      }
      #s <- s + 7
    }
    source("analysis-study-1/prediction_dev/xz-vis-classic_complete.R")
  }

  dist2_prev <- sett_sim_temp$dist2
  
  # Adjust plotting speed
  if(sett_proc$real_time) {

    time_s_diff <- dat_test$time_s[dat_test$pxx_dti_m_rnd1 == s] - dat_test$time_s[dat_test$pxx_dti_m_rnd1 == (s - 1)]
    time_s_diff <- max(0, time_s_diff, na.rm = T)
    #print(time_s_diff)
    #time_s_diff <- dat_test$time_s[s] - dat_test$time_s[s-7]
    Sys.sleep(time_s_diff)
  }
  



  
  
  #ani.record()
  #x <- recordPlot()
  # P_O_Hi <- idm_createSimDat(
  #   list(j = length(sett_bn$states$I),
  #        k = length(sett_bn$states$V),
  #        l = length(sett_bn$states$A)), "", prefix = "")
  # P_O_Hi <- P_O_Hi[sett_bn$idorder]
  # #weight <- (as.numeric(results) / sett_bn$prior$I) * (as.numeric(results) - sett_bn$prior$I)
  # #weight <- round(results, 2) - sett_bn$prior$I#rep(0.25, 4) - as.numeric(results)
  # #print(weight)
  # #sett_bn$prior$I <- sett_bn$prior$I + weight * sett_bn$prior$I
  # #print(sum(sett_bn$prior$I))
  # weight <- results / sett_bn$prior$I
  # weigth <- weight / sum(weight)
  # print(sum(weight))
  # sett_bn$prior$I <- sett_bn$prior$I + weight * (as.numeric(results) - sett_bn$prior$I)
  # print(sum(sett_bn$prior$I))
  # bn <- predLiebner_initBN("V1", sett_bn)
  # coll4prior <- data.table::rbindlist(list(coll4prior, data.frame(s, t(sett_bn$prior$I))))
  
  s <- s + sett_proc$carryout_step
}
outputProcTime(ptm2)

if (sett_proc$append_results)
  coll_overall <- 
    rbind(coll_overall, 
          data.frame(passing = sett_dat$case,
                     coll4results))
