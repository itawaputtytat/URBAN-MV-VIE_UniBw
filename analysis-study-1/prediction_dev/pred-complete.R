ptm <- proc.time()

## Initialise temporary simulation settings
sett_sim_temp <- c()
sett_sim_temp$dist2 <- sett_proc$carryout_am1
sett_sim_temp$dist2_prev <- NA
sett_sim_temp$am_1st <- NA

## Initialise collector for simulation tails
dat_sim_tails_coll <- c()

## Initialise collector for prediction results
dat_pred_results <- array(sett_bn$prior$I, dimnames = list(sett_bn$states$I))
dat_pred_results_coll <- data.frame(sett_proc$carryout_am1, t(dat_pred_results))
names(dat_pred_results_coll) <- c(sett_dat$col_name_am, sett_bn$states$I)

## Initialise collector for changing priors
#dat_prior_coll <- c()
coll_overall <- c()


if (sett_proc$plot) {
  ## Initiliase visualization
  source("analysis-study-1/prediction_dev/_plot-template_complete.R")

  # if (sett_proc$plot_live) {
  # 
  # } else {
  # 
  # }

}

## Loop through passing


while (T) { 

  ## End loop if AM limit is reached
  if (sett_sim_temp$dist2 >= sett_proc$carryout_am2)
    break
  
  ## Find data values corresponding to current prediction setting
  sett_sim_temp <- 
    modifyList(sett_sim_temp, 
               predLiebner_getStartVal4Sim(dat_test, sett_dat, sett_sim_temp))
  
  ## If no data before time lag could be found: 
  ## Skip current prediction process
  if (sett_sim_temp$time1 < (min(dat_test$time_s) - sett_sim$timelag_s)) {
    if (sett_proc$timelag_message)
      messageWithSepLine(c(paste("* Simulation at:", sett_sim_temp$dist2, "failed"), 
                           "(No data available before time lag"))
    sett_sim_temp$dist2 <- sett_sim_temp$dist2 + sett_proc$carryout_step
    next
  } 
  
  ## If data before time lag could be found: 
  ## Remember AM
  if (is.na(sett_sim_temp$am_1st))
    sett_sim_temp$am_1st <- sett_sim_temp$dist2

  ## Run prediction
  source("analysis-study-1/prediction_dev/pred.R")
  
  ## Collect prediction results over all passings
  dat_pred_results_coll <- 
    data.table::rbindlist(list(dat_pred_results_coll,
                               data.frame(sett_sim_temp$pos4carryout_precise, 
                                          t(dat_pred_results))))
  

  ## Extract simulation values
  dat_sim_tails <- lapply(dat_sim, function(hyp)
    lapply(hyp, function(var)
      #tail(var, 1)) )
      if (is.null(var)) NA else
        tail(var, 1)) )
  dat_sim_tails <- as.data.frame(data.table::rbindlist(dat_sim_tails, idcol = T, fill  = T))
  
  ## Collect tails of simulated speed profiles (history / trace)
  if (sett_vis$plot_simulation_history) {
    for (i in seq_along(temp$.id)) { 
      finder <- temp$.id[i]
      dat_sim_tails_coll[[finder]] <- rbind(dat_sim_tails_coll[[finder]], temp[i,2:3])
    }
  }
  
  if (sett_proc$plot)
    source("analysis-study-1/prediction_dev/_plot_complete.R")

  sett_sim_temp$dist2_prev <- sett_sim_temp$dist2
  
  if (sett_proc$stepwise_pause) {
    source("analysis-study-1/prediction_dev/_pred_stepwise_pause.R")
  } else {
    sett_sim_temp$dist2 <- sett_sim_temp$dist2 + sett_proc$carryout_step
  }
  
  # Adjust plotting speed
  if (sett_proc$real_time) {

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
  # dat_prior_coll <- data.table::rbindlist(list(dat_prior_coll, data.frame(s, t(sett_bn$prior$I))))
  
}
outputProcTime(ptm)

if (!sett_proc$append_results) {
  outputString(paste("Shut down all open graphic devices? [y/n]"))
  input <- readline(">>> ")
  if (input == "y") {
    graphics.off()
  }
}


if (sett_proc$append_results)
  dat_pred_results_coll_overall <- 
    rbind(dat_pred_results_coll_overall, 
          data.frame(passing = sett_dat$case,
                     dat_pred_results_coll))
