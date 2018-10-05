
# Settings ----------------------------------------------------------------

## Initiliase settings
sett_clust <- c()

## Data
sett_clust$meta$df_name <- "dat_synth"
sett_clust$filters$pxx <- sett_synth$filters$pxx
#sett_clust$filters$condition_speed <- sett_synth$filters$condition_speed
sett_clust$filters$condition_speed <- c(50, 70)

## Column names
sett_clust$col_names$id <- "passing"
sett_clust$col_names$subject_id <- "subject_id"
sett_clust$col_names$pxx <- "pxx"
sett_clust$col_names$condition_run <- "condition_run"
sett_clust$col_names$condition_speed <- "condition_speed"
sett_clust$col_names$am <- "dti_m_rnd1"
sett_clust$col_names$measure <- "speed_ms_u_smooth"

## Cluster
sett_clust$cluster$k <- 3
sett_clust$cluster$procedure <- "kmeanspp"
sett_clust$cluster$algorithm <- "Hartigan-Wong"
sett_clust$cluster$diss_measure <- "DTWARP"

## Database
sett_clust$db$db_name <- "URBAN-MV-VIE_UniBw_Study-2"
sett_clust$db$conn_name <- dbFindConnObj(sett_clust$db$db_name, output = F)

## Filter
sett_clust$thresholds$am_for_cluster_order <- -50

## Plot
sett_clust$show_plot <- TRUE

## Save
sett_clust$save <- TRUE

## Create table name for data base
sett_clust$db$target_name$prefix <- "t_pred_dsm"
sett_clust$db$target_name$pxx <- 
  sprintf("p%02d", sett_clust$filters$pxx)
sett_clust$db$target_name$condition_speed <- 
  paste0("v", sett_clust$filters$condition_speed, collapse = "_")
sett_clust$db$target_name$main <- paste0("k", sett_clust$cluster$k)
sett_clust$db$target_name$am <- 
  ifelse(grepl("dti", sett_clust$col_names$am), 
         "aggr_dti",
         "aggr_tti")
sett_clust$db$target_name$suffix <- 
  paste_(sett_clust$db$target_name$am, 
         sett_clust$col_names$measure)
sett_clust$db$target_name$final <- 
  paste_(sett_clust$db$target_name$prefix,
         sett_clust$db$target_name$pxx,
         sett_clust$db$target_name$condition_speed,
         sett_clust$db$target_name$main,
         sett_clust$db$target_name$suffix)



# Get data ----------------------------------------------------------------

dat <- 
  get(sett_clust$meta$df_name)

## Filter for AM will affect the cluster result
## Use -100 for p02 in Study 2
#dat_cluster <- dat
dat_cluster <-
  dat %>%
  filter_(paste(sett_clust$col_names$am, ">=", -50))



# Cluster speed profiles --------------------------------------------------

## Prepare data:
## Select relevant variables
## Create new rows for each speed variable
dat_cluster_wide <- 
  dat_cluster %>%
  select_(.dots = 
            c(sett_clust$col_names$id,
              sett_clust$col_names$am,
              sett_clust$col_names$measure)) %>% 
  spread_(sett_clust$col_names$am, sett_clust$col_names$measure) %>% 
  column_to_rownames(sett_clust$col_names$id)

# dat_clust_coll <- c()
# for (k in 1:10) {
#   dat_cluster_results <-
#     clust(
#       dat_cluster_wide,
#       k = k,
#       procedure = sett_clust$cluster$procedure,
#       algorithm = sett_clust$cluster$algorithm,
#       measure4diss = sett_clust$cluster$diss_measure)
#   dat_clust_coll[[k]] <- dat_cluster_results$result
# }
# 
# dat_sil_coll <- c()
# for (k in 1:10) {
#   dat_temp <- dat_clust_coll[[k]]
#   D <- daisy(dat_cluster_wide)
#   dat_sil_coll[[k]] <- silhouette(dat_temp$cluster, D) 
#   #plot(dat_sil_coll[[k]], col=1:k, border=NA)
# }

# dat_total_within_ss_coll <- c()
# for (k in 1:10) {
#   dat_cluster_results <-
#     clust(
#       dat_cluster_wide,
#       k = k, 
#       procedure = sett_clust$cluster$procedure,
#       algorithm = sett_clust$cluster$algorithm,
#       measure4diss = sett_clust$cluster$diss_measure)
#   dat_total_within_ss_coll[[k]] <- dat_cluster_results$result$tot.withinss
# }
# dat_total_within_ss_coll_df <-
#   data.frame(k = 1:10,
#              total_within_ss = dat_total_within_ss_coll)
# 
# # Generate a data frame containing both k and tot_withinss
# 
# # Plot the elbow plot
# ggplot(dat_total_within_ss_coll_df, aes(x = k, y = total_within_ss)) +
#   geom_line() +
#   scale_x_continuous(breaks = 1:10)


#dat_cluster_wide_bck <- dat_cluster_wide

#dat_cluster_wide <- stats::dist(dat_cluster_wide_bck, method = "euclidean")

## Cluster data
dat_cluster_results <-
  clust(
    dat_cluster_wide,
    k = sett_clust$cluster$k, 
    procedure = sett_clust$cluster$procedure,
    algorithm = sett_clust$cluster$algorithm,
    measure4diss = sett_clust$cluster$diss_measure)

## Rename id column
dat_cluster_results$assignment <- 
  dat_cluster_results$assignment %>% 
  rename_at("id", funs(paste0(sett_clust$col_names$id)))



# Postprocess data --------------------------------------------------------

## Join data and cluster groups
## Convert cluster groups to factor
dat_cluster <- 
  left_join(dat_cluster, 
            dat_cluster_results$assignment)
dat_cluster$cluster_group <- factor(dat_cluster$cluster_group)

# Compute cluster centers
dat_cluster_centers <-
  dat_cluster %>%
  group_by_("cluster_group", sett_clust$col_names$am) %>%
  summarize_(.dots = setNames(list(
    interp(~ mean(v),
           v = as.name(sett_clust$col_names$measure))),
    sett_clust$col_names$measure)) %>% 
  ungroup() 

## Order cluster centers
dat_cluster_center_order <- 
  dat_cluster_centers %>% 
  group_by_("cluster_group") %>% 
  filter_(
    paste(sett_clust$col_names$am, "==", 
          sett_clust$thresholds$am_for_cluster_order)) %>% 
  filter(row_number() == 1) %>% 
  arrange_(paste0("desc(", sett_clust$col_names$measure, ")")) %>% 
  ungroup() %>% 
  mutate(cluster_group_ordered = factor(row_number())) %>% 
  select(cluster_group, cluster_group_ordered)

## Join data and ordered cluster numbers
dat_cluster <- 
  left_join(dat_cluster, 
            dat_cluster_center_order)
dat_cluster_centers <- 
  left_join(dat_cluster_centers, 
            dat_cluster_center_order)

dat_cluster_meta <- 
  dat_cluster %>% 
  distinct_(sett_clust$col_names$id, 
            sett_clust$col_names$subject_id,
            sett_clust$col_names$pxx,
            sett_clust$col_names$condition_run,
            sett_clust$col_names$condition_speed,
            "cluster_group", 
            "cluster_group_ordered")



# Fake cluster centers ----------------------------------------------------

## Join data to original data (not filtered data)
## Join data and ordered cluster numbers
dat_cluster <- 
  dat %>% 
  left_join(dat_cluster_meta %>% 
              distinct_(sett_clust$col_names$id,
                        "cluster_group",
                        "cluster_group_ordered"))

dat_cluster_centers <- 
  dat_cluster %>% 
  group_by_(sett_clust$col_names$am, 
            "cluster_group_ordered") %>% 
  summarize_(.dots = setNames(list(
    interp(~ mean(v),
           v = as.name(sett_clust$col_names$measure))),
    sett_clust$col_names$measure))



# Visualize cluster results -----------------------------------------------

if (sett_clust$show_plot) {
  
  plot_cluster <- 
    ggplot() + 
    geom_line(data = dat_cluster,
              aes_string(x = sett_clust$col_names$am,
                         y = sett_clust$col_names$measure,
                         group = sett_clust$col_names$id,
                         color = "cluster_group_ordered"),
              alpha = 0.25) +
    geom_line(data = dat_cluster_centers,
              aes_string(x = sett_clust$col_names$am,
                         y = sett_clust$col_names$measure,
                         group = "cluster_group_ordered",
                         color = "cluster_group_ordered"),
              size = 2) +
    coord_cartesian(xlim = c(-100, 50),
    #coord_cartesian(xlim = c(-50, 10),
                    ylim = c(0, 30)) + 
    scale_color_brewer(palette = "Set1")
  
  plot(plot_cluster)
  
}



# Write cluster centers to database ---------------------------------------

if (sett_clust$save) {
  
  dbWriteTable(get(sett_clust$db$conn_name),
               sett_clust$db$target_name$final,
               dat_cluster_centers,
               row.names = FALSE,
               overwrite = TRUE)
  
  dbWriteTable(get(sett_clust$db$conn_name),
               paste_(sett_clust$db$target_name$final, "meta"),
               dat_cluster_meta,
               row.names = FALSE,
               overwrite = TRUE)
  
}
