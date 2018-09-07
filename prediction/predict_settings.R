
# Settings: Data ----------------------------------------------------------

sett_dat <- c()
sett_dat$df_name <- "dat_study1_t_adtf_pxx_full_aggr_dti_rnd1_intrpld_cut"
sett_dat$col_name_am <- "dti_m_rnd1"
sett_dat$col_name_group <- "passing"
sett_dat$col_name_time <- "time_s"
sett_dat$col_name_dist <- "driven_distance_m"
sett_dat$col_name_speed <- "speed_ms"
sett_dat$col_name_acc_lon <- "acc_lon_ms2"

sett_dat$case <- "p04_stress_s08"
sett_dat$cases <- 
  #unique(get(sett_dat$df_name)[, sett_dat$col_name_group])[c(8, 12, 38, 52, 83)]
  sort(unique(get(sett_dat$df_name)[, sett_dat$col_name_group]))
sett_dat$am_limit1 <- -75
sett_dat$am_limit2 <- 25




# Settings: Procedure -----------------------------------------------------

sett_proc <- c()
sett_proc$stepwise_pause <- F
sett_proc$stepwise_proc_time <- F
sett_proc$plot <- F
sett_proc$plot_live <- F
sett_proc$real_time <- F
sett_proc$append_results <- T
sett_proc$carryout_am_single <- -25
sett_proc$carryout_am1 <- -75
sett_proc$carryout_am2 <- 20
sett_proc$carryout_step <- 1
sett_proc$timelag_message <- F



# Settings: Simulation ----------------------------------------------------

source("fun_Liebner_2013/settings/sett_sim.R")
sett_sim$objpos[2] <- 0
sett_sim$objpos[4] <- 4

## Init hypothesis



# Settings: Visualization -------------------------------------------------

sett_vis <- c()
sett_vis$sim$xmin <- -75
sett_vis$sim$xmax <- 25
sett_vis$sim$ymin <- 0
sett_vis$sim$ymax <- 20
sett_vis$plot_simulation_history <- F



# Settings: Misc ----------------------------------------------------------

source("fun_Liebner_2013/settings/sett_bn.R")
#sett_bn$prior$V <- c(0.01, 0.98, 0.01)

source("fun_Liebner_2013/settings/sett_dsm.R")
sett_dsm$objname4dsm <- "dat_dsm.spread"
#sett_dsm$objname4dsm <- "dat4dsm.spread_v2"
source("analysis-study-1/prediction_dev/init-dsm.R")

source("fun_Liebner_2013/settings/sett_algo.R")
source("fun_Liebner_2013/settings/sett_idm.R")


