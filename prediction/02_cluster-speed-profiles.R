
# Settings ----------------------------------------------------------------

## Initiliase settings
sett_clust <- c()

## Data
sett_clust$meta$df_name <- "dat_synth"

## Column names
sett_clust$col_names$id <- "passing"
sett_clust$col_names$am <- "dti_m_rnd1"
sett_clust$col_names$speed <- "speed_ms"
sett_clust$col_names$measure <- sett_synth$col_names$speed_u_smooth

## Cluster
sett_clust$cluster$k <- 3
sett_clust$cluster$procedure <- "kmeanspp"
sett_clust$cluster$algorithm <- "Hartigan-Wong"
sett_clust$cluster$diss_measure <- "DTWARP"

## Database
sett_clust$db$db_name <- "URBAN-MV-VIE_UniBw_Study-2"
sett_clust$db$conn_name <- dbFindConnObj(sett_clust$db$db_name, output = F)

## Create table name for data base
sett_clust$db$target_name_prefix <- "t_pred_study2"
sett_clust$db$target_name_pxx <- 
  sprintf("p%02d", sett_synth$filters$pxx)
sett_clust$db$target_name_condition_speed <- 
  paste0("v", sett_synth$filters$condition_speed)
sett_clust$db$target_name_main <- "cc"
sett_clust$db$target_name_suffix <- 
  paste_(sett_clust$col_names$am, 
         sett_clust$col_names$measure)
sett_clust$db$target_name <- 
  paste_(sett_clust$db$target_name_prefix,
         sett_clust$db$target_name_pxx,
         sett_clust$db$target_name_condition_speed,
         sett_clust$db$target_name_main,
         sett_clust$db$target_name_suffix)

## Plot
sett_clust$show_plot <- FALSE



# Get data ----------------------------------------------------------------

dat_cluster <- 
  get(sett_clust$meta$df_name)



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

## Rename column for speed
# dat_cluster_centers <- 
#   dat_cluster_centers %>%  
#   rename_at(sett_clust$col_names$measure, funs(sett_clust$col_names$speed))



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
                    ylim = c(0, 30))
  
  plot(plot_cluster)
  
}



# Write cluster centers to database ---------------------------------------

dbWriteTable(get(sett_clust$db$conn_name),
             sett_clust$db$target_name,
             dat_cluster_centers,
             row.names = FALSE,
             overwrite = TRUE)
