
# Preparatory settings ----------------------------------------------------

sett_clust <- c()
sett_clust$df_name <- "dat_idm"
sett_clust$col_name_am <- sett_query$col_name_am
#sett_clust$col_name_speed <- "speed_ms"
sett_clust$col_name_speed <- "speed_ms_u_smoothed"
sett_clust$col_name_group <- "passing"
sett_clust$col_name_round <- "round_txt"
sett_clust$k <- 3
sett_clust$procedure = "kmeanspp" #kmeans, kmeanspp, hclust
sett_clust$algorithm = "Hartigan-Wong"
sett_clust$diss_method <- "DTWARP"
sett_clust$am_limit <- sett_synth$am_limit
sett_clust$show_plot <- F



# Prepare data for processing ---------------------------------------------

dat_init <- 
  get(sett_clust$df_name) %>% 
  data.frame()

dat_init <- 
  dat_init %>% 
  ## Select only necessary variables
  select_(.dots = c(sett_clust$col_name_group,
                    sett_clust$col_name_round,
                    sett_clust$col_name_am,
                    sett_clust$col_name_speed))

## Create wide data format
## Column for each row in arrival measure
dat_init_wide <-
  dat_init %>%
  select_(paste("-", sett_clust$col_name_round)) %>% 
  ## Create new rows for each speed variable
  gather_("col_name_speed", sett_clust$col_name_speed, sett_clust$col_name_speed) %>%
  select_("-col_name_speed") %>%
  spread_(sett_clust$col_name_am, sett_clust$col_name_speed)

## Clustering-algorithms require "clean" dataframes
## ... without additional variables
## ... so identifier column (e.g. passing) has to be removed
## But: Information is backuped in rownames before deleting
rownames(dat_init_wide) <- 
  dat_init_wide[, sett_clust$col_name_group]
dat_init_wide[, sett_clust$col_name_group] <- NULL



# Run cluster algorithm ---------------------------------------------------

## In case of missing preceding or following values
## ... find section with complete data
## Necessary when clustering time-based-data
#min <- min(which(sapply(dat_init_wide.filtered, function(x) { sum(is.na(x)) }) == 0))
#max <- max(which(sapply(dat_init_wide.filtered, function(x) { sum(is.na(x)) }) == 0))

dat_clust_result <-
  #clust2groups(dat_init_wide.filtered[, min:max],
  clust2groups(dat_init_wide,
               k = sett_clust$k, 
               sett_clust$procedure,
               algorithm = sett_clust$algorithm,
               measure4diss = sett_clust$diss_method
               )

## Print assignments
print(dat_clust_result$assignment)



# Merge with data ---------------------------------------------------------

# Convert passing variable to character for merging
dat_clust_result$assignment$passing <-
  as.character(dat_clust_result$assignment$passing)

## Join data and cluster assignment
dat_clust <- 
  left_join(dat_init, 
            dat_clust_result$assignment) %>% 
  data.frame()



# Visualize clustered desired velocity profiles ---------------------------

plot_clust <-
  ggplot() +
  geom_line(data = dat_clust,
            aes_string(x = sett_clust$col_name_am,
                       y = sett_clust$col_name_speed,
                       group = sett_clust$col_name_group,
                       colour = "factor(cluster_group)"),
            size = 1,
            alpha = 0.35) +
  geom_vline(xintercept = sett_clust$am_limit, 
             linetype = "dotted") +
  coord_cartesian(ylim = c(  0, 25)) +
  scale_x_continuous(expand = c(0,0)) +
  ggtitle(paste(sett_clust$procedure,
                sett_clust$algorithm))+ 
  theme_bw()

if (sett_clust$show_plot)
  plot(plot_clust)



# Compute cluster centres -------------------------------------------------

dat_clust_centers <-
  dat_clust %>%
  # select_("cluster_group",
  #         sett_clust$col_name_speed,
  #         sett_clust$col_name_am,
  #         sett_clust$col_name_group,
  #         sett_clust$col_name_round) %>%
  mutate(cluster_group = as.numeric(cluster_group)) %>% 
  #group_by_("cluster_group", sett_clust$col_name_dist) %>%
  group_by_("cluster_group", sett_clust$col_name_am) %>%
  ## Variable name is temporary, see next step
  summarise_(.dots = 
               setNames(list(interp(~ mean(v), 
                                    v = as.name(sett_clust$col_name_speed))),
                        paste_(sett_clust$col_name_speed, "avg"))) %>% 
  data.frame()



# Visualise general (ungrouped) cluster centres ---------------------------

plot_clustcenters <-
  plot_clust +
  geom_line(data = dat_clust_centers,
            aes_string(x = sett_clust$col_name_am,
                       y = paste_(sett_clust$col_name_speed, "avg"),
                       colour = "factor(cluster_group)"),
            size = 2) 

if (sett_clust$show_plot)
  plot(plot_clustcenters)



# Order DSM by speed ------------------------------------------------------

clustcentres_order <- 
  dat_clust_centers %>% 
  filter_(paste(sett_clust$col_name_am, "==", 
                min(dat_clust_centers[, sett_clust$col_name_am]))) %>% 
  group_by_(.dots = lapply("cluster_group", as.symbol)) %>%
  summarise_(speed_ms_initial = 
               paste("mean(", 
                     paste_(sett_clust$col_name_speed, "avg"), 
                     ")") ) %>% 
  arrange(desc(speed_ms_initial)) %>% 
  mutate(clust_group_ordered = row_number()) %>% 
  data.frame()



# Visualize ordered cluster centres ---------------------------------------

dat_clust <- 
  left_join(dat_clust,
            clustcentres_order %>% 
              select(cluster_group, 
                     clust_group_ordered)) %>% 
  mutate(cluster_group = clust_group_ordered) %>% 
  mutate(clust_group_ordered = NULL)

dat_clust_centers <- 
  left_join(dat_clust_centers,
            clustcentres_order %>% 
              select(cluster_group, 
                     clust_group_ordered)) %>% 
  mutate(cluster_group = clust_group_ordered) %>% 
  mutate(clust_group_ordered = NULL)
  
plot_clustcenters_ordered <-
  ggplot() +
  geom_line(data = dat_clust,
            aes_string(x = sett_clust$col_name_am,
                       y = sett_clust$col_name_speed,
                       group = sett_clust$col_name_group,
                       colour = "factor(cluster_group)"),
            size = 1,
            alpha = 0.35) +
  geom_line(data = dat_clust_centers,
            aes_string(x = sett_clust$col_name_am,
                       y = paste_(sett_clust$col_name_speed, "avg"),
                       colour = "factor(cluster_group)"),
            size = 2) +
  geom_vline(xintercept = sett_clust$am_limit, 
             linetype = "dotted") +
  coord_cartesian(ylim = c(  0, 25)) +
  scale_x_continuous(expand = c(0,0)) +
  ggtitle(paste(sett_clust$procedure,
                sett_clust$algorithm))+ 
  theme_bw()

if (sett_clust$show_plot)
  plot(plot_clustcenters_ordered)
