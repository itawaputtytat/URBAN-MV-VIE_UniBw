
# Load case ---------------------------------------------------------------

source("prediction/load-case-data.R")



# Initialise prediction framework -----------------------------------------

source("prediction/initialise-prediction-framework.R")



# Start timer -------------------------------------------------------------

ptm <- proc.time()



# Initialise components ---------------------------------------------------

## Temporary simulation settings
sett_sim_temp <- c()
sett_sim_temp$am2 <- sett_pred$carryout_am_start
sett_sim_temp$am2_prev <- sett_pred$carryout_am_start
sett_sim_temp$am_1st <- NA

## Collector for simulation tails
dat_sim_tails_coll <- c()

## Collector for prediction results
dat_pred_results <- sett_bn$priors$I
dat_pred_results_coll <- 
  data.frame(sett_pred$carryout_am_start, 
             t(dat_pred_results))
names(dat_pred_results_coll) <- c(sett_case$col_names$am, sett_bn$state_names$I)

## Collector for changing priors
#dat_prior_coll <- c()
coll_overall <- c()

## Visualization
if (sett_plot$plot) {
  sett_plot$pred_is_single <- FALSE
  source("prediction/resources_plot/_plot-template_complete.R")
}



# Find start values for simulation ----------------------------------------

## End loop if AM limit is reached
#while (sett_sim_temp$am2 <= sett_pred$carryout_am_end) { 
while(T) {
  
  ## End loop if AM limit is reached
  if (sett_sim_temp$am2 >= sett_pred$carryout_am_end) {
    break
  }
  
  ## Find start values for simulation
  sett_sim_temp <- 
    modifyList(sett_sim_temp, 
               predLiebner_findSimStartValues(
                 dat_case, 
                 ## AM selection fifferent from single prediction
                 am_carryout = sett_sim_temp$am2,
                 col_name_am = sett_case$col_names$am,
                 col_name_time = sett_case$col_names$time,
                 col_name_speed = sett_case$col_names$speed,
                 time_lag_s = sett_sim$time_lag_s))
  
  ## If no data before time lag could found: Remember AM
  if (sett_sim_temp$failed) {
    sett_sim_temp$am_1st <- sett_sim_temp$am2
  }

  ## Run prediction
  source("prediction/pred.R")

  ## Collect prediction results
  dat_pred_results_coll <- 
    data.table::rbindlist(list(
      dat_pred_results_coll,
      data.frame(sett_sim_temp$am2, t(dat_pred_results))))
  
  ## Collect tails of simulated speed profiles for history plots
  if (sett_pred$collect_sim_tails) {
    for (i in seq_along(dat_sim_tails$.id)) { 
      finder <- dat_sim_tails$.id[i]
      dat_sim_tails_coll[[finder]] <- 
        rbind(dat_sim_tails_coll[[finder]], dat_sim_tails[i,2:3])
    }
  }
  
  if (sett_plot$plot) {
    source("prediction/resources_plot/_plot_complete.R")
  }
  
  sett_sim_temp$am2_prev <- sett_sim_temp$am2
  
  if (sett_pred$pause_stepwise) {
    source("prediction/_pred_stepwise_pause.R")
  } else {
    sett_sim_temp$am2 <- sett_sim_temp$am2 + sett_pred$carryout_am_step
  }
  
  # Adjust plotting speed
  if (sett_plot$real_time) {

    ## Compute time difference
    sett_sim_temp$time_now <- 
      dat_test[, sett_case$col_names$am] == sett_sim_temp$am2
    sett_sim_temp$time_now_m1s <- 
      dat_test[sett_case$col_names$am] == (sett_sim_temp$am2 - 1)
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

if (sett_pred$append_results) {
  
  sett_pred$meta_info <- c()
  
  ## Version and case
  sett_pred$meta_info$dsm <- sett_dsm$version
  sett_pred$meta_info$version <- sett_pred$bn_version
  sett_pred$meta_info$passing <- sett_case$case_id
  
  ## Node info for driving style
  finder <- which(sett_pred$bn_evidence$nodes == "DS")
  sett_pred$meta_info["evidence_ds"] <-
    ifelse(length(which(sett_pred$bn_evidence$nodes == "DS")), 
           sett_pred$bn_evidence$states[finder], 
           NA)
  
  ## Node info for stress
  finder <- which(sett_pred$bn_evidence$nodes == "ST")
  sett_pred$meta_info["evidence_st"] <-
    ifelse(length(which(sett_pred$bn_evidence$nodes == "ST")), 
           sett_pred$bn_evidence$states[finder], 
           NA)
  
  # dat_pred_results_coll_overall <-
  #   rbind(dat_pred_results_coll_overall,
  #         data.frame(passing = sett_case$case_id,
  #                    version = sett_pred$bn_version,
  #                    evidence = sett_pred$bn_evidence$print,
  #                    dat_pred_results_coll))
  
  dat_pred_results_coll_overall <-
    rbind(dat_pred_results_coll_overall,
          merge(data.frame(sett_pred$meta_info),
                data.frame(dat_pred_results_coll))) 
}
#   
# 
# if (!sett_pred$append_results) {
#   outputString(paste("Shut down all open graphic devices? [y/n]"))
#   input <- readline(">>> ")
#   if (input == "y") {
#     graphics.off()
#   }
# }


