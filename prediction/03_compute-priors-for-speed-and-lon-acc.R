
# Preparatory settings ----------------------------------------------------

## Initiliase settings
sett_priors <- c()

## Data
sett_priors$meta$df_name <- "dat_cluster"

## Database
sett_priors$db$db_name <- "URBAN-MV-VIE_UniBw_Study-2"
sett_priors$db$conn_name <- dbFindConnObj(sett_priors$db$db_name, output = F)
sett_priors$db$src_names$dat_acc_lon_max <- 
  "t_adtf_results_p02_dti_m_rnd1_acc_lon_max_aggr"
# Replace position reference with filter settings for pxx
sett_priors$db$src_names$dat_acc_lon_max <-
  sub("p\\d{2}",
      sprintf("p%02d", sett_synth$filters$pxx),
      sett_priors$db$src_names$dat_acc_lon_max)

## Column names
sett_priors$col_names$id <- "passing"
sett_priors$col_names$subject_id <- "subject_id"
sett_priors$col_names$condition_speed <- "condition_speed"
sett_priors$col_names$speed_measure <- sett_synth$col_names$speed_u_smooth
sett_priors$col_names$am <- "dti_m_rnd1"
sett_priors$col_names$acc_lon_max <- "mean"
sett_priors$col_names$cluster_group <- "cluster_group_ordered"

## Filters
sett_priors$filters$condition_speed <- sett_synth$filters$condition_speed

## Thresholds
sett_priors$tresholds$acc_lon_max <- c(1.5, 2)

## Plot
sett_priors$show_plot <- FALSE

## Create table name for data base
sett_priors$db$target_name_prefix <- "t_pred_study2"
sett_priors$db$target_name_pxx <- sett_clust$db$target_name_pxx
sett_priors$db$target_name_condition_speed <- 
  sett_clust$db$target_name_condition_speed
sett_priors$db$target_name_main <- "priors"
sett_priors$db$target_name_suffix_speed <- 
  paste_(sett_priors$col_names$am,
         "speed_models")
sett_priors$db$target_name_suffix_acc <- 
  paste_("acc_lon_max")
sett_priors$db$target_name_speed <- 
  paste_(sett_priors$db$target_name_prefix,
         sett_priors$db$target_name_pxx,
         sett_priors$db$target_name_condition_speed,
         sett_priors$db$target_name_main,
         sett_priors$db$target_name_suffix_speed)
sett_priors$db$target_name_acc <- 
  paste_(sett_priors$db$target_name_prefix,
         sett_priors$db$target_name_pxx,
         sett_priors$db$target_name_condition_speed,
         sett_priors$db$target_name_main,
         sett_priors$db$target_name_suffix_acc)



# Get data ----------------------------------------------------------------

## Speed
dat_Mk <- get(sett_priors$meta$df_name)

## Maximum longitudinal acceleration
dat_al_Mk <- 
  dbGetSrc(sett_priors$db$conn_name, 
           sett_priors$db$src_names$dat_acc_lon_max)



# Preprocess data ---------------------------------------------------------

## Add cluster groups to acceleration data
dat_al_Mk <- 
  dat_al_Mk %>% 
  filter_(createFilterString(
    sett_priors$col_names$condition_speed, 
    sett_priors$filters$condition_speed)) %>% 
  left_join(dat_Mk %>% 
              select_(
                sett_priors$col_names$subject_id,
                sett_priors$col_names$cluster_group))



# Compute P(Mk) -----------------------------------------------------------

## Compute rate
dat_prob_Mk <- 
  predLiebner_compProb_Mk(
    dat_Mk, 
    col_name_cluster_group = sett_priors$col_names$cluster_group,
    show_plot = sett_priors$show_plot)

## Extract rate
## Rename values
dat_prob_Mk_vector <- dat_prob_Mk$results$rate
names(dat_Mk_prob) <- dat_prob_Mk$results$M

## Save plot
plot_prob_Mk <- dat_prob_Mk$plot_dat



# Compute P(al|Mk) --------------------------------------------------------

## Compute rate
dat_prob_al_Mk <- 
  predLiebner_compProb_al_Mk(
    dat_al_Mk, 
    col_name_id = sett_priors$col_names$subject_id,
    col_name_cluster_group = sett_priors$col_names$cluster_group,
    col_name_acc_lon_max = sett_priors$col_names$acc_lon_max,
    thresholds_acc = sett_priors$tresholds$acc_lon_max,
    show_plot = sett_priors$show_plot)

## Extract rate
## Rename values
dat_prob_al_Mk_vector <- dat_prob_al_Mk$results$rate
names(dat_prob_al_Mk_vector) <- 
  paste_(dat_prob_al_Mk$results$M,
         dat_prob_al_Mk$results$a)

## Save plot
plot_prob_al_Mk <- dat_prob_al_Mk$plot_dat



# Visualise P(Mk) and P(al|Mk) --------------------------------------------

if (sett_priors$show_plot) {
  plot_prob <- arrangeGrob(plot_prob_Mk, plot_prob_al_Mk, nrow = 1)
  plot(plot_prob)
}



# Write DSM to database ---------------------------------------------------

## Priors for cluster groups
dbWriteTable(get(sett_priors$db$conn_name),
             sett_priors$db$target_name_speed,
             dat_prob_Mk$results,
             row.names = FALSE,
             overwrite = TRUE)

## Priors for acceleration
dbWriteTable(get(sett_priors$db$conn_name),
             sett_priors$db$target_name_acc,
             dat_prob_al_Mk$results,
             row.names = FALSE,
             overwrite = TRUE)
