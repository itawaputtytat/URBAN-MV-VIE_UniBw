
#### NEXT: COMPUTE PRIORS IN INIT PHASE


# Settings: Test data -----------------------------------------------------

sett_case <- c()

## Data
sett_case$df_name <- "dat_study2_t_adtf_pxx_full_aggr_dti_rnd1_intrpld_cut"
#sett_case$df_name <- "dat_study2_t_adtf_pxx_full_aggr_tti_rnd1_intrpld_cut"

## Column names
sett_case$col_names$id <- "passing"
sett_case$col_names$am <- "dti_m_rnd1"
#sett_case$col_names$am <- "tti_s_rnd1"
sett_case$col_names$am_plot <- 
  ifelse(grepl("tti", sett_case$col_names$am), 
         "dti_m", 
         "dti_m_rnd1")
sett_case$col_names$time <- "time_s"
sett_case$col_names$driven_distance <- "driven_distance_m"
sett_case$col_names$speed <- "speed_ms"
sett_case$col_names$acc_ <- "acc_lon_ms2"

## Case selection
sett_case$case_id <- "p02_s02_t1_v50"
sett_case$case_ids <- unique(get(sett_case$df_name)[, sett_case$col_names$id])
sett_case$case_ids <- sort(sett_case$case_ids)[1]

## Filter
sett_case$filter$am1 <- -100
sett_case$filter$am2 <- -25



# Settings: DSM -----------------------------------------------------------

sett_dsm <- c()

## Database
sett_dsm$db$db_name <- "URBAN-MV-VIE_UniBw_Study-2"
sett_dsm$db$conn_name <- dbFindConnObj(sett_dsm$db$db_name, output = F)
sett_dsm$db$src_names$dsm <- 
  "t_pred_study2_p02_v50_cc_dti_m_rnd1_speed_ms_u_smooth"
sett_dsm$db$src_names$priors_speed <- 
  "t_pred_study2_p02_v50_priors_dti_m_rnd1_speed_models"
sett_dsm$db$src_names$priors_acc_lon_max <- 
  "t_pred_study2_p02_v50_priors_acc_lon_max"

## Column names
sett_dsm$col_names$cluster_group <- "cluster_group"
sett_dsm$col_names$am <- "dti_m_rnd1"
sett_dsm$col_names$speed <- "speed_ms_u_smooth"

## Thresholds:
## Maximum DSM (m/s)
## AM threshold for setting maximum DSM (m)
## Maximum longitudinal acceleration (m/s²)
sett_dsm$thresholds$u_max <- c(48/3.6, 54/3.6, 60/3.6)
sett_dsm$thresholds$u_max_am <- 10
sett_dsm$thresholds$acc_lon_max <- c(1.5, 2, 2.5)



# Settings: Simulation ----------------------------------------------------

## time_lag_s:        Time lag (data basis for simulation) (s)
## obj_pos:           Position for virtual obstacle
## cut_curves:        Complete curves family vs. cut
## cutcurves_time_s:  Duration of curves family
## compute_intention: Presence of intention

sett_sim <- c()
sett_sim$time_lag_s <- 1
sett_sim$time_sim_s <- sett_sim$time_lag_s
#sett_sim$obj_pos <- c(-999, -9, -40, 7)   
sett_sim$obj_pos <- c(-999, 0, -999, 0)   
sett_sim$cut_curves <- T              
sett_sim$cut_curves_time_s <- 1       
sett_sim$compute_I <- c(T, T, T, T)



# Settings: Procedure -----------------------------------------------------

#sett_proc$plot_live <- F
#sett_proc$timelag_message <- F

sett_pred <- c()
sett_pred$model$study <- "study2"
sett_pred$model$pxx <- "2"
sett_pred$carryout_am_start <- -75
sett_pred$carryout_am_end <- 20
sett_pred$carryout_am_step <- 2
sett_pred$print_ptm_stepwise <- FALSE
sett_pred$append_results <- TRUE
sett_pred$pause_stepwise <- FALSE
sett_pred$sigma_s_m <- 1.2 # 1.2; 1; 0.6; 1.8; 1.5 * 0.8; 1.5; 1.2
sett_pred$sigma_v_ms <- 1.2 # 1.2; 1; 0.6; 1.8; 2 * 0.8; 2; 1.7

## Needs to be enabled for plotting simulation tails
sett_pred$collect_sim_tails <- TRUE 

## BN Version
sett_pred$bn_version <- "Liebner"
sett_pred$bn_version <- "stress2"



# Visualization -----------------------------------------------------------

sett_plot <- c()
sett_plot$plot <- TRUE
sett_plot$xmin <- -75
sett_plot$xmax <- 25
sett_plot$ymin <- 0
sett_plot$ymax <- 20
sett_plot$colors$dsm <- c("#ED212450", "#6ABD4550", "#3953A450")
sett_plot$plot_sim_tails_history <- FALSE
sett_plot$real_time <- FALSE



# Initialise prediction framework -----------------------------------------

source("prediction/initialise-prediction-framework.R")



# Prediction for single prediction at single point ------------------------

## Overwrite setting for carryout if needed
sett_pred$carryout_am_start <- -35
source("prediction/pred-single.R")



# Prediction for complete case --------------------------------------------

## Overwrite setting for carryout if needed
sett_pred$carryout_am_start <- -75
sett_pred$carryout_am_end <- 20
source("prediction/pred-complete.R")



# Prediction for multiple cases -------------------------------------------

## Overwrite setting for carryout if needed
sett_pred$carryout_am_start <- -75
sett_pred$carryout_am_end <- 20

## Initialise collector for prediction results
dat_pred_results_coll_overall <- c()

## Loop through cases
for (case in sett_case$case_ids) {
  outputString(paste("* Currently processing:", case))
  source("prediction/pred-complete.R")
}


