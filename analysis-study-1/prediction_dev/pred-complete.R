
# Start timer -------------------------------------------------------------

ptm <- proc.time()


# Initialise components ---------------------------------------------------

## Collector for simulation tails
dat_sim_tails_coll <- c()

## Collector for prediction results
dat_pred_results <- array(sett_bn$prior$I, dimnames = list(sett_bn$states$I))
dat_pred_results_coll <- data.frame(sett_proc$carryout_am1, t(dat_pred_results))
names(dat_pred_results_coll) <- c(sett_dat$col_name_am, sett_bn$states$I)

## Collector for changing priors
#dat_prior_coll <- c()
coll_overall <- c()

## Visualization
if (sett_proc$plot) {
  source("analysis-study-1/prediction_dev/_plot-template_complete.R")
}



# Find start values for simulation ----------------------------------------

## End loop if AM limit is reached
while (sett_sim_temp$am2 >= sett_proc$carryout_am2) { 
  
  ## Find start values for simulation
  sett_sim_temp <- 
    modifyList(sett_sim_temp, 
               predLiebner_findSimStartValues(
                 dat_test, 
                 am_carryout = sett_pred$carryout_am1,
                 col_name_am = sett_case$col_names$am,
                 col_name_time = sett_case$col_names$time,
                 col_name_speed = sett_case$col_names$speed,
                 time_lag_s = sett_sim$time_lag_s))
  
  sett_sim_temp$am2 <- sett_sim_temp$am2 + sett_proc$carryout_step

  ## If no data before time lag could found: Remember AM
  if (sett_sim_temp$failed) {
    sett_sim_temp$am1 <- sett_sim_temp$am2
  }

  ## Run prediction
  source("analysis-study-1/prediction_dev/pred.R")

  ## Collect prediction results
  dat_pred_results_coll <- 
    data.table::rbindlist(list(
      dat_pred_results_coll,
      data.frame(sett_pred$carryout_am1, t(dat_pred_results))))

  ## Extract simulation values
  ## ... for all hypothesis
  ## ... and both variables speed and distance
  dat_sim_tails <- 
    lapply(dat_sim, function(hyp)
      lapply(hyp, function(var) {
        if (is.null(var)) {
          NA
        } else {
          tail(var, 1)
        }
      }))
  dat_sim_tails <- 
    as.data.frame(
      data.table::rbindlist(dat_sim_tails, 
                            idcol = T, 
                            fill  = T))
  
  ## Collect tails of simulated speed profiles for history plots
  if (sett_pred$collect_sim_tails) {
    for (i in seq_along(dat_sim_tails$.id)) { 
      finder <- temp$.id[i]
      dat_sim_tails_coll[[finder]] <- 
        rbind(dat_sim_tails_coll[[finder]], 
              temp[i,2:3])
    }
  }
  
  if (sett_plot$plot) {
    source("analysis-study-1/prediction_dev/_plot_complete.R")
  }
  
  if (sett_plot$stepwise_pause) {
    source("analysis-study-1/prediction_dev/_pred_stepwise_pause.R")
  } else {
    sett_sim_temp$am2 <- sett_sim_temp$am2 + sett_pred$carryout_am_step
  }
  
  # Adjust plotting speed
  if (sett_vis$real_time) {

    ## Compute time difference
    sett_sim_temp$time_now <- dat_test[, sett_case$col_names$am] == s
    sett_sim_temp$time_now_m1s <- dat_test[sett_case$col_names$am] == (s - 1)
    time_s_diff <- 
      dat_test$time_s[sett_sim_temp$time_now] - 
      dat_test$time_s[sett_sim_temp$time_now_m1s]
    
    ## Take maximum time difference
    time_s_diff <- max(0, time_s_diff, na.rm = T)

    ## Set visualization inactive
    Sys.sleep(time_s_diff)
  }
}


# Stop timer --------------------------------------------------------------

outputProcTime(ptm)



# Going on ... ------------------------------------------------------------

if (sett_proc$append_results) {
  dat_pred_results_coll_overall <- 
    rbind(dat_pred_results_coll_overall, 
          data.frame(passing = sett_dat$case,
                     dat_pred_results_coll))
}
  

if (!sett_pred$append_results) {
  outputString(paste("Shut down all open graphic devices? [y/n]"))
  input <- readline(">>> ")
  if (input == "y") {
    graphics.off()
  }
}


