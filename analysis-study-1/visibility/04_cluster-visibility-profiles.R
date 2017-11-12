
## Cluster visibility profiles

# Preparatory settings ----------------------------------------------------

sett_dat <- c()
sett_dat$db_conn_name <- dbFindConnObj("Study-1")
sett_dat$src_name <- "t_visibility_smoothed"
sett_dat$col_names$position = "position_id"
sett_dat$col_names$am <- "pxx_dti_m_rnd1"

sett_proc <- c()
sett_proc$col_names$value_left <- "width_intersection_center_to_left_perc"
sett_proc$col_names$value_right <- "width_intersection_center_to_right_perc"

sett_proc$clust$target <- 
  c(sett_proc$col_names$value_left,
    sett_proc$col_names$value_right)

sett_proc$clust$proc <- "kmeans"
sett_proc$clust$algo <- "Hartigan-Wong"
sett_proc$clust$method_diss <- "DTW"
sett_proc$clust$method_aggl <- "average"
sett_proc$clust$k <- 3



# Load data ---------------------------------------------------------------

dat <- dbGetSrc(sett_dat$db_conn_name, sett_dat$src_name)



# Preprocess data ---------------------------------------------------------

## Reshape data into long format 
## Actually only necessary when selecting more than one direction
dat_long <-
  dat %>%
  gather_("direction", "percentage",
          gather_cols = c(sett_proc$col_names$value_left,
                          sett_proc$col_names$value_right)) %>% 
  filter_(paste("direction", "%in%", deparse(quote(sett_proc$clust$target)), 
                collapse = "|"))


## Recode direction values
dat_long$direction <- 
  ifelse(grepl("left", dat_long$direction),
         "left", "right")


## Create new passing variable
dat_long <- 
  dat_long %>% 
  mutate_(.dots = setNames(list(
    interp(~ paste_(sprintf("p%02d", v), w),
           v = as.name(sett_dat$col_names$position),
           w = as.name("direction"))),
    "case")) %>% 
  select(-one_of(sett_dat$col_names$position, "direction"))


## Find cases to remove (no changes in visibility, only zeros)
case_finder <-
  dat_long %>%
  group_by_("case") %>%
  summarise(mean = mean(percentage)) %>%
  filter(mean == 0 | mean == 100) %>%
  select_("case")

case_finder <- unname(unlist(case_finder))


## Remove cases from data
dat_long <- dat_long %>% filter_(paste0("!", "case", "%in%", "case_finder"))


## Reshape
dat_long_wide <- spread(dat_long, key = pxx_dti_m_rnd1, value = percentage)


## Use case column as row names and remove case column
## Necessary for cluster algorithms
rownames(dat_long_wide) <- dat_long_wide[, "case"]
dat_long_wide[, "case"] <- NULL



# Clustering --------------------------------------------------------------

## Use clustering framework
dat_clust_results <-
  clust(dat_wide = dat_long_wide,
        k = sett_proc$clust$k,
        procedure = sett_proc$clust$proc,
        algorithm = sett_proc$clust$algo,
        method4agglo = sett_proc$clust$method_aggl,
        measure4diss = sett_proc$clust$method_diss)

## Get group assignments
dat_clust_assignments <- dat_clust_results$assignment

# Convert passing variable to character for potential merging
#test1$case <- as.character(dat_clust_assignments$passing)
#dat_long$case <- as.character(dat_long$case)

## Join data and cluster assignment
dat_long_clustered <- 
  left_join(dat_long, 
            dat_clust_assignments, 
            by = setNames("id", "case"))



# Visualization -----------------------------------------------------------

plotdat.clust <-
  ggplot(dat_long_clustered) +
  geom_line(aes_string(x = sett_dat$col_names$am,
                       y = "percentage",
                       group = "case",
                       colour = "clustgroup"),
            size = 1.25,
            alpha = 0.15) +
  # stat_summary(aes_string(x = "sxx_dist_m_rnd1",
  #                         y = "percentage"),
  #              geom = "line",
  #              fun.y = "mean",
  #              colour = "black",
  #              size = 2) +
  stat_summary(aes_string(x = sett_dat$col_names$am,
                          y = "percentage",
                          group = "clustgroup",
                          colour = "clustgroup"),
               geom = "line",
               fun.y = "mean",
               size = 2) +
  coord_cartesian(xlim = c(-50, 0),
                  ylim = c(0, 105)) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  ggtitle(paste(sett_proc$clust$target, sett_proc$clust$proc, sep = ",")) +
  theme_bw() +
  guides(colour = F)

plot(plotdat.clust)

