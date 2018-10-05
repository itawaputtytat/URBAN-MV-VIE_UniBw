
# Settings ----------------------------------------------------------------

## Data base
sett_class <- c()
sett_class$db$db_name <- "URBAN-MV-VIE_UniBw_Study-2"
sett_class$db$conn_name <- dbFindConnObj(sett_class$db$db_name, output = F)

## Source names
sett_class$db$src_names$acc_lon_max$p01 <- "t_adtf_results_p01_dti_m_rnd1_acc_lon_summary"
sett_class$db$src_names$acc_lon_max$p02 <- "t_adtf_results_p02_dti_m_rnd1_acc_lon_summary"
sett_class$db$src_names$acc_lon_max$p03 <- "t_adtf_results_p03_dti_m_rnd1_acc_lon_summary"
sett_class$db$src_names$acc_lat_max$p02 <- "t_adtf_results_p02_dti_m_rnd1_acc_lat_summary"
sett_class$db$src_names$acc_lat_max$p03 <- "t_adtf_results_p03_dti_m_rnd1_acc_lat_summary"
sett_class$db$src_names$acc_lat_max$p05 <- "t_adtf_results_p05_dti_m_rnd1_acc_lat_summary"
sett_class$db$src_names$acc_lat_max$p06 <- "t_adtf_results_p06_dti_m_rnd1_acc_lat_summary"
sett_class$db$src_names$dec_lon_max$p02 <- "t_adtf_results_p02_dti_m_rnd1_dec_lon_summary"
sett_class$db$src_names$dec_lon_max$p03 <- "t_adtf_results_p03_dti_m_rnd1_dec_lon_summary"
sett_class$db$src_names$dec_lon_max$p05 <- "t_adtf_results_p05_dti_m_rnd1_dec_lon_summary"
sett_class$db$src_names$dec_lon_max$p06 <- "t_adtf_results_p06_dti_m_rnd1_dec_lon_summary"
sett_class$db$src_names$speed_avg$p02 <- "t_adtf_results_p02_dti_m_rnd1_speed_summary"
sett_class$db$src_names$speed_avg$p03 <- "t_adtf_results_p03_dti_m_rnd1_speed_summary"
sett_class$db$src_names$speed_sd$p02 <- "t_adtf_results_p02_dti_m_rnd1_speed_summary"
sett_class$db$src_names$speed_sd$p03 <- "t_adtf_results_p03_dti_m_rnd1_speed_summary"
sett_class$db$src_names$vip <- "t_experimental_conditions_vip"

## Column names
sett_class$col_names$subject_id <- "subject_id"
sett_class$col_names$condition_speed <- "condition_speed"
sett_class$col_names$condition_run <- "condition_run"



# Load data ---------------------------------------------------------------

dat_col_names_coll <- c()
for (measure in names(sett_class$db$src_names)) {
  for (condition in names(sett_class$db$src_names[[measure]])) {
    src_name <- sett_class$db$src_names[[measure]][[condition]]
    #print(measure)
    dat_temp <- dbGetSrc(sett_class$db$conn_name, src_name)
    obj_name <- paste_("dat", measure, condition)
    
    
    dat_col_names_temp <- paste_(condition, measure)
    dat_col_names_coll <- c(dat_col_names_coll, dat_col_names_temp)
    
    if (!grepl("speed", measure)) {
     
      if (grepl("acc", measure)) {
        
        dat_temp <- 
          dat_temp %>% 
          select(subject_id, condition_speed, condition_run, "max") %>% 
          rename_(.dots = setNames("max", dat_col_names_temp))
        
      } else {
        dat_temp <-
          dat_temp %>%
          select(subject_id, condition_speed, condition_run, "min") %>%
          rename_(.dots = setNames("min", dat_col_names_temp))
      }
     
      ## Speed 
    } else {
      
      if (grepl("avg", measure)) {
        
        dat_temp <-
          dat_temp %>%
          select(subject_id, condition_speed, condition_run, "mean") %>%
          rename_(.dots = setNames("mean", dat_col_names_temp))
        
      } else {
        
        dat_temp <-
          dat_temp %>%
          select(subject_id, condition_speed, condition_run, "sd") %>%
          rename_(.dots = setNames("sd", dat_col_names_temp))
        
      }
    }
    
    assign(obj_name, dat_temp)
    print(obj_name)
  }
}

dat_vip <- dbGetSrc(sett_class$db$conn_name, sett_class$db$src_names$vip)



# Join data ---------------------------------------------------------------

## Prepare template
dat_all_template <- 
  dat_speed_avg_p02 %>% 
  distinct_(sett_class$col_names$subject_id,
            sett_class$col_names$condition_speed,
            sett_class$col_names$condition_run)

## Join P01
dat_all <- dat_all_template

# dat_all <-
#   dat_all %>%
#   left_join(dat_acc_lon_max_p01)

## Join P02
dat_all <- 
  dat_all %>% 
  # left_join(dat_speed_avg_p02) %>%
  # # left_join(dat_speed_sd_p02) %>%
  # # left_join(dat_dec_lon_max_p02) %>%
  # left_join(dat_acc_lon_max_p02) %>%
  left_join(dat_acc_lat_max_p02)

# Join P03
dat_all <-
  dat_all %>%
  # left_join(dat_speed_avg_p03) %>%
  # # left_join(dat_speed_sd_p03) %>%
  # # left_join(dat_dec_lon_max_p03) %>%
  # left_join(dat_acc_lon_max_p03) %>%
  left_join(dat_acc_lat_max_p03)

# # Join P05
# dat_all <-
#   dat_all %>%
#   left_join(dat_dec_lon_max_p05) %>%
#   left_join(dat_acc_lat_max_p05)
# 
# 
# # Join P06
# dat_all <-
#   dat_all %>%
#   left_join(dat_dec_lon_max_p06) %>%
#   left_join(dat_acc_lat_max_p06)


## Filter data
#dat_all <- dat_all %>% filter(condition_speed == 50) %>% filter(condition_run == 1)
#dat_all <- dat_all %>% filter(condition_speed == 50)
#dat_all <- dat_all %>% filter(condition_run == 1)

  
dat_all_spread <- 
  dat_all %>% 
  gather(variable, value, 
         setdiff(names(dat_all), 
                 c(sett_class$col_names$subject_id, 
                   sett_class$col_names$condition_speed,
                   sett_class$col_names$condition_run))) %>% 
  mutate_(.dots = setNames(list(
    interp(~ paste_(var, paste0("v", v), paste0("r", r)),
           var = as.name("variable"),
           v = as.name(sett_class$col_names$condition_speed),
           r = as.name(sett_class$col_names$condition_run))),
    "variable")) %>%
  select_(.dots = 
            paste0("-", c(sett_class$col_names$condition_speed,
                          sett_class$col_names$condition_run))) %>% 
  spread(variable, value)



# PCA ---------------------------------------------------------------------

## Remove unneeded columns
col_finder <- 
  colnames(dat_all_spread) %in% 
  c("subject_id", "condition_speed", "condition_run")
col_finder <- names(dat_all_spread)[!col_finder]
#dat_all_spread_scaled <- dat_all_spread[, col_finder]
dat_all_spread_scaled <- data.frame(scale(dat_all_spread[, col_finder]))

## Run PCA
dat_pca <- prcomp(dat_all_spread_scaled, scale = T)
summary(dat_pca)

## Calculate variance and proportion of explained variance
dat_pca_var <- dat_pca$sdev^2
dat_pca_pve <- dat_pca_var / sum(dat_pca_var)

## Correlation matrix
dat_all_spread_scaled_cor <- cor(dat_all_spread_scaled)
dat_all_spread_scaled_eigen <- eigen(dat_all_spread_scaled_cor)

## Eigenvalues
dat_all_spread_scaled_cor <- cor(dat_all_spread_scaled)
#round(dat_all_spread_scaled_cor, 2)
dat_all_spread_scaled_cor_eigen <- eigen(dat_all_spread_scaled_cor)
dat_all_spread_scaled_cor_eigen$values



# Visualize PCA results ---------------------------------------------------

## Variance
barplot(dat_pca_var, 
        xlab = "Principal Component",
        ylab = "Variance")

# Plot variance explained for each principal component
plot(dat_pca_pve, 
     xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     ylim = c(0, 1), 
     type = "b")

# Plot cumulative proportion of variance explained
plot(cumsum(dat_pca_pve), 
     xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     ylim = c(0, 1), 
     type = "b")
axis(side = 1, 
     at = 1:10)
abline(a = .85, 
       b = 0, 
       col= "red")

#biplot(dat_pca, choices = c(1,10), scale = 0)    

## Alternative plots using factoextra
# library(factoextra)
# fviz_pca_ind(dat_pca,
#              col.ind = "cos2", # Color by the quality of representation
#              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
#              repel = TRUE     # Avoid text overlapping
# )
# 
# fviz_pca_var(dat_pca,
#              col.var = "contrib", # Color by contributions to the PC
#              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
#              repel = TRUE     # Avoid text overlapping
# )



# Extract components ------------------------------------------------------

#https://www.r-bloggers.com/pca-and-k-means-clustering-of-delta-aircraft/
dat_pca_comp <- dat_pca$x[,1:2]



# Cluster analysis --------------------------------------------------------

## Collect cluster results for 1 to 10 clusters
## Initialise collectors
dat_cluster_coll <- c()
dat_total_within_ss_coll <- c()
dat_sil_coll <- c()

## For trying cluster analysis without PCA
#dat_pca_comp <- dat_all_spread[, col_finder]

## Run cluster analysis
for (k in 2:10) {
  
  ## Cluster analysis
  dat_cluster_coll[[k]] <- kmeanspp(dat_pca_comp, k = k, iter.max = 500, nstart=25)
  
  ## Calculate total within SS
  dat_total_within_ss_coll[[k]] <- dat_cluster_coll[[k]]$tot.withinss
  
  ## Compute silhouette coefficient
  ## Plot silhouette
  D <- daisy(dat_pca_comp)
  dat_sil_coll[[k]] <- silhouette(dat_cluster_coll[[k]]$cluster, D)
  plot(dat_sil_coll[[k]], col = 1:k, border = NA)
}

## Preprocess total within sum of squares
dat_total_within_ss_coll_df <-
  data.frame(k = 2:10, 
             total_within_ss = dat_total_within_ss_coll)

## Scree plot
ggplot(dat_total_within_ss_coll_df, 
       aes(x = k, y = total_within_ss)) +
  geom_line() +
  scale_x_continuous(breaks = 2:10)

# dist <- stats::dist(dat_pca_comp, method = "euclidean")
# dat_hcluster_coll <- hclust(dist)
# plot(dat_hcluster_coll)
# sil <- silhouette(cutree(dat_hcluster_coll, 5), dist)
# plot(sil)


# Visualize cluster with data ---------------------------------------------

#dat_cluster_coll_wo_pca <- dat_cluster_coll
#plot(dat_all_scaled, col=dat_cluster_coll[[3]]$cluster, pch = 16)
#windows(); plot(dat_pca_comp, col=dat_cluster_coll[[3]]$cluster, pch = 16)
#windows(); plot(dat_all[, !col_finder], col=dat_cluster_coll[[3]]$cluster, pch = 16)

## Join cluster groups with subject ids
dat_all_spread$cluster_group <- dat_cluster_coll[[2]]$cluster

windows()
plot(dat_all_spread[, col_finder], 
     col = dat_all_spread$cluster_group, 
     pch = 16)

dat_all_gather <- 
  dat_all %>% 
    gather(key = variable,
           value = value,
           -one_of("subject_id", "condition_speed", "condition_run")) %>% 
  group_by(subject_id, condition_speed, variable) %>% 
  summarize(value = mean(value)) %>% 
  mutate(pxx = ifelse(grepl("p02", variable), 2, 3)) %>%
  mutate(variable = ifelse(grepl("lat", variable), "lat", "lon")) %>%
  spread(variable, value) %>% 
  left_join(dat_all_spread %>% 
              select(subject_id,
                     cluster_group))
  
dat_test <- 
  dat_all_gather %>% 
  filter(pxx == 3)
windows()
plot(dat_test$lon, dat_test$lat, 
     col = dat_all_spread$cluster_group, 
     pch = 16)



# fdsfsd ------------------------------------------------------------------

# another option
makePairs <- function(data) 
{
  grid <- expand.grid(x = 1:ncol(data), y = 1:ncol(data))
  grid <- subset(grid, x != y)
  all <- do.call("rbind", lapply(1:nrow(grid), function(i) {
    xcol <- grid[i, "x"]
    ycol <- grid[i, "y"]
    data.frame(xvar = names(data)[ycol], yvar = names(data)[xcol], 
               x = data[, xcol], y = data[, ycol], data)
  }))
  all$xvar <- factor(all$xvar, levels = names(data))
  all$yvar <- factor(all$yvar, levels = names(data))
  densities <- do.call("rbind", lapply(1:ncol(data), function(i) {
    data.frame(xvar = names(data)[i], yvar = names(data)[i], x = data[, i])
  }))
  list(all=all, densities=densities)
}

# expand iris data frame for pairs plot
gg1 = makePairs(dat_all_spread[,-c(1,10)])

# new data frame mega iris
mega_dat = data.frame(gg1$all, cluster=rep(dat_all_spread$cluster, length=nrow(gg1$all)))

# pairs plot
ggplot(mega_dat, aes_string(x = "x", y = "y")) + 
  facet_grid(xvar ~ yvar, scales = "free") + 
  geom_point(aes(colour=as.factor(cluster)), na.rm = TRUE, alpha=0.8) + 
  stat_density(aes(x = x, y = ..scaled.. * diff(range(x)) + min(x)), 
               data = gg1$densities, position = "identity", 
               colour = "grey20", geom = "line")




# Calculate proportions ---------------------------------------------------

dat_all_summary <-
  dat_all %>% 
  left_join(dat_all_spread %>% 
              select(subject_id, cluster_group)) %>% 
  group_by(subject_id, condition_speed) %>% 
  filter(condition_run == 2) %>% 
  group_by(cluster_group) %>% 
  summarize(n = n(),
            #speed_avg_avg = mean(p02_speed_avg),
            #acc_lon_max_avg = mean(p02_acc_lon_max),
            acc_lat_max_avg = mean(p02_acc_lat_max)) %>% 
  mutate(prop = n / sum(n),
         n = NULL) %>%
  ungroup() %>% 
  # arrange(speed_avg_avg) %>% 
  # mutate(speed_avg_avg_order = row_number()) %>% 
  # arrange(acc_lon_max_avg) %>% 
  # mutate(acc_lon_order = row_number()) %>% 
  arrange(acc_lat_max_avg) %>% 
  mutate(acc_lat_order = row_number()) %>% 
  mutate(cluster_group_ordered = acc_lat_order) %>% 
  # mutate(cluster_group_ordered = 
  #          (acc_lon_order + 
  #             acc_lat_order + 
  #             speed_avg_avg_order) / 3) %>% 
  ## Reverse order
  #mutate(cluster_group_ordered = 4 - cluster_group_ordered) %>% 
  data.frame()

dat_all_meta <- 
  dat_all_spread %>% 
  select(subject_id, cluster_group) %>% 
  left_join(dat_all_summary %>% 
              select(cluster_group,
                     cluster_group_ordered))



# Write to database -------------------------------------------------------

dbWriteTable(get(sett_class$db$conn_name),
             "t_driving_style_k2",
             dat_all_meta,
             row.names = FALSE,
             overwrite = TRUE)


# 
# 
# # Parallel plots ----------------------------------------------------------
# 
# 
# 
# test <- 
#   dat_all_template %>% 
#   select_(sett_class$col_names$subject_id,
#           sett_class$col_names$condition_run,
#           sett_class$col_names$condition_speed,
#           "cluster2") %>% 
#   mutate(var = paste0("v", condition_speed, "_", "r", condition_run)) %>% 
#   select_(.dots = 
#             paste0("-", c(sett_class$col_names$condition_speed,
#                           sett_class$col_names$condition_run))) %>% 
#   spread(var, cluster2) %>% 
#   mutate(subject_id = factor(subject_id))
# 
# 
# ggplot() + 
#   geom_line(data = test,
#             aes(x = var,
#                 y = as.factor(cluster2),
#                 group = subject_id),
#             position = position_dodge(.5)) + 
#   geom_point(data = test,
#             aes(x = var,
#                 y = as.factor(cluster2),
#                 group = subject_id),
#             position = position_dodge(.5))



# library(NbClust)
# nb <- NbClust(dat_pca_comp, distance = "euclidean", min.nc = 2,
#               max.nc = 10, method = "ward.D2", index ="all")
# # Visualize the result
# library(factoextra)
# fviz_nbclust(nb) + theme_minimal()
# 
# km.res <- eclust(dat_all_scaled, "kmeans", k = 2,
#                  nstart = 25, graph = FALSE)
# 
# # Visualize k-means clusters
# fviz_cluster(km.res, geom = "point", frame.type = "norm")
# par(mfrow = c(1, 1))






