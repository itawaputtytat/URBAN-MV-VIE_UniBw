
# Start timer -------------------------------------------------------------

ptm <- proc.time()



# Find start values for simulation ----------------------------------------

sett_sim_temp <-
  predLiebner_findSimStartValues(
    dat_test, 
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
  print(sett_plot$pred_is_single)
  source("analysis-study-1/prediction_dev/_plot-template_single.R")
  source("analysis-study-1/prediction_dev/_plot_single.R")
}
