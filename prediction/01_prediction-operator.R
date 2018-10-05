
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
sett_case$col_names$acc_lon_ms2<- "acc_lon_ms2"

## Case selection
sett_case$case_id <- "p02_s06_t1_v50"
sett_case$case_ids_all <- unique(get(sett_case$df_name)[, sett_case$col_names$id])
sett_case$case_ids_all <- sort(sett_case$case_ids_all)

## Filter
sett_case$filter$am1 <- -100
sett_case$filter$am2 <- -25



# Settings: DSM -----------------------------------------------------------

sett_dsm <- c()

## Database
sett_dsm$db$db_name <- "URBAN-MV-VIE_UniBw_Study-2"
sett_dsm$db$conn_name <- dbFindConnObj(sett_dsm$db$db_name, output = F)

## DSM version
sett_dsm$version <- "clustering"
#sett_dsm$version <- "curvature"
sett_dsm$db$src_names$dsm <- 
  "t_pred_dsm_p03_v50_v70_k3_aggr_dti_speed_ms_u_smooth"
sett_dsm$db$src_names$curvature <- "t_curv_p03_aggr_dti"

## Priors
## pxx will be replaced in initialition by correct pxx based on case id
sett_dsm$db$src_names$priors_speed <- 
  "t_pred_priors_p03_v50_v70_dti_m_rnd1_dsm"
sett_dsm$db$src_names$priors_acc_lon_max <- 
  "t_pred_priors_p03_v50_v70_acc_lon_max"


## Column names
sett_dsm$col_names$cluster_group <- "cluster_group_ordered"
sett_dsm$col_names$condition_speed <- "condition_speed"
sett_dsm$col_names$driving_style <- "driving_style"
sett_dsm$col_names$am <- "dti_m_rnd1"
sett_dsm$col_names$speed <- "speed_ms_u_smooth"

## Thresholds:
## Maximum DSM (m/s)
## AM threshold for setting maximum DSM (m)
## Maximum longitudinal acceleration (m/s²)
sett_dsm$thresholds$u_max <- c(48/3.6, 54/3.6, 60/3.6)
#sett_dsm$thresholds$u_max <- c(48/3.6, 54/3.6, 60/3.6, 80/3.6)
sett_dsm$thresholds$u_max_am <- 10
sett_dsm$thresholds$acc_lon_max <- c(1.5, 2, 2.5)
sett_dsm$thresholds$gradients <- c(0.15, 0.2, 0.25)



# Driving style -----------------------------------------------------------

sett_ds <- c()

## Driving styles
sett_ds$n_ds <- 3
if (sett_ds$n_ds == 3) { 
  sett_ds$ds_names <- c("sporty", "moderate", "comfortable")
}
if (sett_ds$n_ds == 2) { 
  sett_ds$ds_names <- c("sporty", "moderate")
}

## Database
sett_ds$db$db_name <- "URBAN-MV-VIE_UniBw_Study-2"
sett_ds$db$conn_name <- dbFindConnObj(sett_ds$db$db_name, output = F)

## Data
sett_ds$db$src_names$dat_ds <- paste0("t_driving_style_k", sett_ds$n_ds)
sett_ds$col_names$class <- "cluster_group_ordered"



# Settings: Simulation ----------------------------------------------------

## time_lag_s:        Time lag (data basis for simulation) (s)
## obj_pos:           Position for virtual obstacle
## cut_curves:        Complete curves family vs. cut
## cutcurves_time_s:  Duration of curves family
## compute_intention: Presence of intention

sett_sim <- c()
sett_sim$time_lag_s <- 1
sett_sim$time_sim_s <- sett_sim$time_lag_s
#sett_sim$obj_pos <- c(-999, -15, -999, -5)   
sett_sim$obj_pos <- c(-999, -999, -999, -999)   
## Does not work yet
sett_sim$cut_curves <- T              
sett_sim$cut_curves_time_s <- 1       
sett_sim$compute_I <- c(T, T, T, T)



# Visualization -----------------------------------------------------------

sett_plot <- c()
sett_plot$plot <- TRUE
sett_plot$xmin <- -75
sett_plot$xmax <- 25
sett_plot$ymin <- 0
sett_plot$ymax <- 30
sett_plot$plot_sim_tails_history <- FALSE
sett_plot$real_time <- FALSE



# Settings: Prediction ----------------------------------------------------

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
#sett_pred$bn_version <- "A_ST"
#sett_pred$bn_version <- "A_S_ST"
#sett_pred$bn_version <- "A_S_DS_ST"

## Evidence
sett_pred$bn_evidence$overwrite <- FALSE
##
sett_pred$bn_evidence$nodes <- c()
sett_pred$bn_evidence$states <- c()
##
# sett_pred$bn_evidence$nodes <- c("ST")
# sett_pred$bn_evidence$states <- c("stress")
##
#sett_pred$bn_evidence$nodes <- c("DS")
#sett_pred$bn_evidence$states <- c("comfortable")

# sett_pred$bn_evidence$nodes <- c("S")
# sett_pred$bn_evidence$states <- c("k1")
# sett_pred$bn_evidence$nodes <- c("ST")
# sett_pred$bn_evidence$states <- c("stress")
# sett_pred$bn_evidence$nodes <- c("ST")
# sett_pred$bn_evidence$states <- c("no_stress")
#sett_pred$bn_evidence$nodes <- c("ST", "S")
# sett_pred$bn_evidence$states <- c("stress", "k1")
# sett_pred$bn_evidence$nodes <- c("DS")
# sett_pred$bn_evidence$states <- c("sporty")
# sett_pred$bn_evidence$nodes <- c("DS", "ST")
# sett_pred$bn_evidence$states <- c("sporty", "no_stress")



# Prediction for single prediction at single point ------------------------

## Overwrite setting for carryout if needed
sett_case$case_id <- "p02_s25_t2_v50"
sett_pred$bn_version <- "Liebner"
sett_pred$bn_version <- "A_S_DS_ST"
sett_pred$carryout_am_start <- -20
source("prediction/pred-single.R")



# Prediction for complete case --------------------------------------------

## Overwrite setting for carryout if needed
sett_case$case_id <- "p02_s25_t2_v50"
sett_pred$carryout_am_start <- -75
sett_pred$carryout_am_end <- 0
source("prediction/pred-complete.R")



# Prepare history for multiple case prediction ----------------------------

## Initialise collector for prediction results
# if (exists("dat_pred_results_coll_overall") & 
#     length("dat_pred_results_coll_overall") == 0) {
  
  dat_pred_results_coll_overall <- c()
  
# }



# Prediction for multiple complete cases ----------------------------------

## Overwrite setting for carryout if needed
sett_pred$carryout_am_start <- -100
sett_pred$carryout_am_end <- 0

## Set cases
#sett_case$case_ids_all <- grep("p03", sett_case$case_ids_all, value = TRUE)
sett_case$case_ids <- sett_case$case_ids_all
#sett_case$case_ids <- sett_case$case_ids_all[201:320]
#sett_case$case_ids <- "p03_s20_t2_v70"

## Sett DSM version
#sett_dsm$version <- "clustering"

## Version
sett_pred$bn_version_batch <- c("Liebner", "A_S_DS_ST")
#sett_pred$bn_version <- "A_S_DS_ST"

## Plot
sett_plot$plot <- FALSE

## Loop through cases
for (case in sett_case$case_ids[1:10]) {
  for (version in sett_pred$bn_version_batch) {
    sett_pred$bn_version <- version
    catWithSepLine(case)
    outputString(paste("* Currently processing:", case))
    sett_case$case_id <- case
    source("prediction/pred-complete.R")
  }
}



# Explore -----------------------------------------------------------------

dat_pred_results_coll_overall_plot <- 
  dat_pred_results_coll_overall %>% 
  #filter(passing == sett_pred$meta_info$passing) %>% 
  #filter(passing == sett_case$case_ids[10]) %>% 
  mutate(version = paste_(version, dsm)) %>% 
  mutate(pred_id = paste_(version, dsm, evidence_ds, evidence_st)) %>% 
  mutate(states = paste_(evidence_ds, evidence_st)) %>% 
  mutate(states = ifelse(version == "Liebner", "", states))

windows()
ggplot() + 
  geom_line(data = dat_pred_results_coll_overall_plot,
            aes(x = dti_m_rnd1,
                y = Intent3,
                group = pred_id,
                color = version),
            size = 1) + 
  facet_wrap(~passing) + 
  scale_color_brewer(palette = "Set1") +
  coord_cartesian(ylim = c(0, 1)) +
  theme_bw()



# Delete version ----------------------------------------------------------

dat_pred_results_coll_overall <- 
  dat_pred_results_coll_overall[-which(dat_pred_results_coll_overall$version!="Liebner"),] 


# Diff --------------------------------------------------------------------

test <- 
  dat_pred_results_coll_overall %>% 
  #dat_pred_results_coll_overall_p03 %>% 
  mutate(flag = ifelse(dti_m_rnd1 == -100 & Intent1 == .25, 1, 0)) %>% 
  filter(flag == 0) %>% 
  gather(intent, prob, paste0("Intent", c(1:4))) %>% 
  #mutate(evidence = sub("Evidence", "", evidence)) %>% 
  #mutate(model_intent = paste(version, evidence, intent, sep="; ")) %>% 
  mutate(version_intent = paste_(version, intent)) %>% 
  
  distinct(passing, dti_m_rnd1, version_intent, prob) %>% 
  
  select(passing, dti_m_rnd1, version_intent, prob) %>% 
  spread(version_intent, prob) %>% 
  mutate(diff = A_S_DS_ST_Intent3 - Liebner_Intent3) %>% 
  mutate(stress = ifelse(grepl("v50", passing), "no_stress", "stress"))

test_summary <- 
  test %>% 
  group_by(stress, dti_m_rnd1) %>% 
  summarize(diff = mean(diff))

ggplot() + 
  geom_line(data = test,
            aes_string(x = "dti_m_rnd1",
                       y = "diff",
                       group = "passing"),
            color = "darkblue",
            size = 0.5,
            alpha = 0.25) + 
  geom_line(data = test_summary,
            aes_string(x = "dti_m_rnd1",
                       y = "diff"),
            color = "darkblue",
            size = 2) + 
  geom_hline(yintercept = 0,
             color = "red",
             size = 1) + 
  facet_grid(.~stress) 
            
# ggplot() + 
#   geom_line(data = test,
#             aes(x = dti_m_rnd1,
#                 y = A_S_DS_ST_Intent3,
#                 group = passing),
#             color = "darkblue",
#             alpha = 0.25)


# dat_pred_results_coll_overall_p02 <-
#   dat_pred_results_coll_overall %>%
#   filter(grepl("p02", passing))

# dat_pred_results_coll_overall_p03 <- 
#   dat_pred_results_coll_overall
