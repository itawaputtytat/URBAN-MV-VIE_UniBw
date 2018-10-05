
# Preparatory settings ----------------------------------------------------

## Initiliase settings
sett_priors <- c()

## Filters
sett_priors$filters$pxx <- 2
sett_priors$filters$condition_speed <- c(50, 70)
sett_priors$filters$driving_style <- 2

## Database
sett_priors$db$db_name <- "URBAN-MV-VIE_UniBw_Study-2"
sett_priors$db$conn_name <- dbFindConnObj(sett_priors$db$db_name, output = F)

## Source names
sett_priors$db$src_names$speed_profiles <- 
  "t_pred_dsm_p03_v50_v70_k3_aggr_dti_speed_ms_u_smooth_meta"

sett_priors$db$src_names$dat_acc_lon_max <- 
  "t_adtf_results_p03_dti_m_rnd1_acc_lon_summary"

## CHange source names according to pxx filter

sett_priors$db$src_names$speed_profiles <- 
  sub(regmatches(sett_priors$db$src_names$speed_profiles, 
                 regexpr("p\\d\\d", 
                         sett_priors$db$src_names$speed_profiles)),
      sprintf("p%02d", sett_priors$filters$pxx),
      sett_priors$db$src_names$speed_profiles)

sett_priors$db$src_names$dat_acc_lon_max <- 
  sub(regmatches(sett_priors$db$src_names$dat_acc_lon_max, 
                 regexpr("p\\d\\d", 
                         sett_priors$db$src_names$dat_acc_lon_max)),
      sprintf("p%02d", sett_priors$filters$pxx),
      sett_priors$db$src_names$dat_acc_lon_max)

sett_priors$db$src_names$dat_driving_style <- 
  paste0("t_driving_style_k", sett_priors$filters$driving_style)


# Replace position reference with filter settings for pxx
sett_priors$db$src_names$dat_acc_lon_max <-
  sub("p\\d{2}",
      sprintf("p%02d", sett_priors$filters$pxx),
      sett_priors$db$src_names$dat_acc_lon_max)

## Column names
sett_priors$col_names$id <- "passing"
sett_priors$col_names$subject_id <- "subject_id"
sett_priors$col_names$condition_speed <- "condition_speed"
sett_priors$col_names$speed_measure <- "speed_ms_u_smooth"
sett_priors$col_names$am <- "dti_m_rnd1"
sett_priors$col_names$acc_lon_max <- "mean"
sett_priors$col_names$cluster_group <- "cluster_group_ordered"


## Thresholds
sett_priors$tresholds$acc_lon_max <- c(1.5, 2)

## Plot
sett_priors$show_plot <- FALSE

## Create table name for data base
sett_priors$db$target_name$pxx <- sprintf("p%02d", sett_priors$filters$pxx)
sett_priors$db$target_name$condition_speed <- 
  sett_clust$db$target_name$condition_speed
sett_priors$db$target_name$prefix <- 
  paste_("t_pred_priors",
         sett_priors$db$target_name$pxx,
         sett_priors$db$target_name$condition_speed)
sett_priors$db$target_name$suffix_speed <- 
  paste_(sett_priors$col_names$am,
         "dsm")
sett_priors$db$target_name$suffix_acc <- 
  paste_("acc_lon_max")
sett_priors$db$target_name$speed <- 
  paste_(sett_priors$db$target_name$prefix,
         sett_priors$db$target_name$suffix_speed)
sett_priors$db$target_name$acc <- 
  paste_(sett_priors$db$target_name$prefix,
         sett_priors$db$target_name$suffix_acc)



# Get data ----------------------------------------------------------------

## Speed
dat_Mk <- 
  dbGetSrc(sett_priors$db$conn_name, 
           sett_priors$db$src_names$speed_profiles)

## Maximum longitudinal acceleration
dat_al_Mk <- 
  dbGetSrc(sett_priors$db$conn_name, 
           sett_priors$db$src_names$dat_acc_lon_max)

## Driving style
dat_ds <-
  dbGetSrc(sett_priors$db$conn_name,
           sett_priors$db$src_names$dat_driving_style) %>% 
  mutate(driving_style = as.character(cluster_group_ordered))



# Preprocess data ---------------------------------------------------------

dat_Mk <- 
  dat_Mk %>% 
  left_join(dat_ds %>% 
              select(subject_id, driving_style),
            by = "subject_id")



# Compute P(Mk) -----------------------------------------------------------

## Compute rate for each cluster group
dat_prob_Mk <- 
  predLiebner_compProb_Mk(
    dat_Mk, 
    col_name_cluster_group = sett_priors$col_names$cluster_group)

## Compute rate for each cluster group and speed condition
dat_prob_Mk_speed <- 
  predLiebner_compProb_Mk(
    dat_Mk, 
    col_name_cluster_group = sett_priors$col_names$cluster_group,
    #col_name_group = sett_priors$col_names$condition_speed)
    col_name_group = 
      c(sett_priors$col_names$condition_speed))

## Compute rate for each cluster group, speed condition and driving style
dat_prob_Mk_speed_ds <- 
  predLiebner_compProb_Mk(
    dat_Mk, 
    col_name_cluster_group = sett_priors$col_names$cluster_group,
    #col_name_group = sett_priors$col_names$condition_speed)
    col_name_group = 
      c(sett_priors$col_names$condition_speed,
        "driving_style"))

## Extract rate
## Rename values
dat_prob_Mk_vector <- dat_prob_Mk$rate
names(dat_prob_Mk_vector) <- dat_prob_Mk$M

dat_prob_Mk_speed_vector <- dat_prob_Mk_speed$rate
names(dat_prob_Mk_speed_vector) <- 
  paste_(paste0("v", dat_prob_Mk_speed$condition_speed),
         dat_prob_Mk_speed$M)

dat_prob_Mk_speed_ds_vector <- dat_prob_Mk_speed_ds$rate
names(dat_prob_Mk_speed_ds_vector) <- 
  paste_(paste0("v", dat_prob_Mk_speed_ds$condition_speed),
         paste0("ds", dat_prob_Mk_speed_ds$driving_style),
         dat_prob_Mk_speed$M)



# Compute P(al|Mk) --------------------------------------------------------

## Add cluster groups to acceleration data
dat_al_Mk <- 
  dat_al_Mk %>% 
  filter_(createFilterString(
    sett_priors$col_names$condition_speed, 
    sett_priors$filters$condition_speed)) %>% 
  data.frame() %>% 
  left_join(dat_Mk)

## Compute rate
dat_prob_al_Mk <- 
  predLiebner_compProb_al_Mk(
    dat_al_Mk, 
    col_name_id = sett_priors$col_names$subject_id,
    col_name_cluster_group = sett_priors$col_names$cluster_group,
    col_name_acc_lon_max = sett_priors$col_names$acc_lon_max,
    thresholds_acc = sett_priors$tresholds$acc_lon_max)

## Compute rate for each cluster group and speed condition
dat_prob_al_Mk_speed <- 
  predLiebner_compProb_al_Mk(
    dat_al_Mk, 
    col_name_id = sett_priors$col_names$subject_id,
    col_name_cluster_group = sett_priors$col_names$cluster_group,
    col_name_group = sett_priors$col_names$condition_speed,
    col_name_acc_lon_max = sett_priors$col_names$acc_lon_max,
    thresholds_acc = sett_priors$tresholds$acc_lon_max)

## Compute rate for each cluster group, speed condition and driving style
dat_prob_al_Mk_speed_ds <- 
  predLiebner_compProb_al_Mk(
    dat_al_Mk, 
    col_name_id = sett_priors$col_names$subject_id,
    col_name_cluster_group = sett_priors$col_names$cluster_group,
    col_name_group = c(sett_priors$col_names$condition_speed, "driving_style"),
    col_name_acc_lon_max = sett_priors$col_names$acc_lon_max,
    thresholds_acc = sett_priors$tresholds$acc_lon_max)

## Extract rate
## Rename values
dat_prob_al_Mk_vector <- dat_prob_al_Mk$rate
names(dat_prob_al_Mk_vector) <- 
  paste_(dat_prob_al_Mk$M, 
         dat_prob_al_Mk$a)

dat_prob_al_Mk_speed_vector <- dat_prob_al_Mk_speed$rate
names(dat_prob_al_Mk_speed_vector) <- 
  paste_(paste_("v", dat_prob_al_Mk_speed$M),
         dat_prob_al_Mk_speed$M, 
         dat_prob_al_Mk_speed$a)

dat_prob_al_Mk_speed_ds_vector <- dat_prob_al_Mk_speed_ds$rate
names(dat_prob_al_Mk_speed_ds_vector) <- 
  paste_(paste0("v", dat_prob_al_Mk_speed_ds$condition_speed),
         paste0("ds", dat_prob_al_Mk_speed_ds$driving_style),
         dat_prob_al_Mk_speed_ds$M,
         dat_prob_al_Mk_speed_ds$a)



# Write DSM to database ---------------------------------------------------

## Priors for DSM
dbWriteTable(get(sett_priors$db$conn_name),
             sett_priors$db$target_name$speed,
             dat_prob_Mk,
             row.names = FALSE,
             overwrite = TRUE)

dbWriteTable(get(sett_priors$db$conn_name),
             paste_(sett_priors$db$target_name$speed, "speed"),
             dat_prob_Mk_speed,
             row.names = FALSE,
             overwrite = TRUE)

dbWriteTable(get(sett_priors$db$conn_name),
             paste_(sett_priors$db$target_name$speed, "speed", "ds",
                    paste0("k", sett_priors$filters$driving_style)),
             dat_prob_Mk_speed_ds,
             row.names = FALSE,
             overwrite = TRUE)

## Priors for acceleration
dbWriteTable(get(sett_priors$db$conn_name),
             sett_priors$db$target_name$acc,
             dat_prob_al_Mk,
             row.names = FALSE,
             overwrite = TRUE)

dbWriteTable(get(sett_priors$db$conn_name),
             paste_(sett_priors$db$target_name$acc, "speed"),
             dat_prob_al_Mk_speed,
             row.names = FALSE,
             overwrite = TRUE)

dbWriteTable(get(sett_priors$db$conn_name),
             paste_(sett_priors$db$target_name$acc, "speed", "ds",
                    paste0("k", sett_priors$filters$driving_style)),
             dat_prob_al_Mk_speed_ds,
             row.names = FALSE,
             overwrite = TRUE)
