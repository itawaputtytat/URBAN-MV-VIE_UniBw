
# Load case ---------------------------------------------------------------

source("prediction/load-case-data.R")



# Start timer -------------------------------------------------------------

ptm <- proc.time()



# Find start values for simulation ----------------------------------------

sett_sim_temp <-
  predLiebner_findSimStartValues(
    dat_case, 
    am_carryout = sett_pred$carryout_am_start,
    col_name_am = sett_case$col_names$am,
    col_name_time = sett_case$col_names$time,
    col_name_speed = sett_case$col_names$speed,
    time_lag_s = sett_sim$time_lag_s)



# Run prediction ----------------------------------------------------------

source("analysis-study-1/prediction_dev/pred.R")



# Stop timer --------------------------------------------------------------

outputProcTime(ptm)



# Visualize results -------------------------------------------------------

if (sett_plot$plot) {
  ## In case plotting of simulation tails is enabled
  sett_plot$pred_is_single <- TRUE 
  source("prediction/resources_plot/_plot-template_single.R")
  source("prediction/resources_plot/_plot_single.R")
  sett_plot$pred_is_single <- FALSE
}
