
# Settings ----------------------------------------------------------------

## Initiliase settings
sett_dsm <- c()

## Data
sett_dsm$meta$df_name <- "dat_cluster_centers"

## Column names
sett_dsm$col_names$cluster_group <- "cluster_group"
sett_dsm$col_names$am <- "dti_m_rnd1"
sett_dsm$col_names$speed_measure <- sett_synth$col_names$speed_u_smooth

## Database
sett_dsm$db$db_name <- "URBAN-MV-VIE_UniBw_Study-2"
sett_dsm$db$conn_name <- dbFindConnObj(sett_dsm$db$db_name, output = F)

## Create table name for data base
sett_dsm$db$target_name_prefix <- "t_pred_study2"
sett_dsm$db$target_name_pxx <- "p02"
sett_dsm$db$target_name_main <- "dsm"
sett_dsm$db$target_name_suffix <- 
  paste_(sett_dsm$col_names$am, 
         sett_dsm$col_names$speed_measure)
sett_dsm$db$target_name <- 
  paste_(sett_dsm$db$target_name_prefix,
         sett_dsm$db$target_name_pxx,
         sett_dsm$db$target_name_main,
         sett_dsm$db$target_name_suffix)



# Get data ----------------------------------------------------------------

dat_dsm <- get(sett_dsm$meta$df_name)

## Select relevat variables
dat_dsm <- 
  dat_dsm %>% 
  select_(sett_dsm$col_names$cluster_group,
          sett_dsm$col_names$am,
          sett_dsm$col_names$speed_measure) %>% 
  rename_at(sett_dsm$col_names$cluster_group, funs(paste0("k"))) %>% 
  rename_at(sett_dsm$col_names$am, funs(paste0("am"))) %>% 
  rename_at(sett_dsm$col_names$speed_measure, funs(paste0("speed")))

## Add prefix to cluster group numbers
dat_dsm$k <- paste0("k", dat_dsm$k)

## Spread speed values into separate columns for each cluster group
dat_dsm_spread <- dat_dsm %>% spread(k, speed) 

dat_dsm_spread <- 
  predLiebner_initDSM(
    col_name_cluster_group = "cluster_group_ordered",
    col_name_am = sett_clust$col_names$am,
    col_name_speed = sett_clust$col_names$measure,
    threshold_am = 10,
    thresholds_u_max = c(10, 12, 16))



# Write DSM to database ---------------------------------------------------

dbWriteTable(get(sett_dsm$db$conn_name),
             sett_dsm$db$target_name,
             dat_dsm_spread,
             row.names = FALSE,
             overwrite = TRUE)
