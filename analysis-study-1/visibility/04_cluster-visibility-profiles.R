
## Cluster visibility profiles

# Preparatory settings ----------------------------------------------------

## Data 
sett_vis <- c()
sett_vis$db$conn_name <- dbFindConnObj("Study-1")
sett_vis$db$src_name <- "t_visibility_percentage_smoothed"

## Column names
sett_vis$col_names$position = "pxx"
sett_vis$col_names$am <- "dti_m_rnd1"
sett_vis$col_names$value_left <- "width_intersection_center_to_left_perc"
sett_vis$col_names$value_right <- "width_intersection_center_to_right_perc"

# Cluster analysis
sett_vis$cluster$col_names_target <- 
  c(sett_vis$col_names$value_left,
    sett_vis$col_names$value_right)
sett_vis$cluster$proc <- "kmeans"
sett_vis$cluster$algo <- "Hartigan-Wong"
sett_vis$cluster$method_diss <- "DTW"
sett_vis$cluster$method_aggl <- "average"
sett_vis$cluster$k <- 3



# Load data ---------------------------------------------------------------

dat <- 
  dbGetSrc(sett_vis$db$conn_name, 
           sett_vis$db$src_name)



# Preprocess data ---------------------------------------------------------

## Reshape data into long format 
## Actually only necessary when selecting more than one direction
dat_long <-
  dat %>%
  gather_("direction", "percentage",
          gather_cols = 
            c(sett_vis$col_names$value_left,
              sett_vis$col_names$value_right)) %>% 
  filter_(paste("direction", "%in%", 
                deparse(quote(sett_vis$cluster$col_names_target)), 
                collapse = "|"))

## Recode direction values
dat_long$direction <- 
  ifelse(grepl("left", dat_long$direction),
         "left", 
         "right")

## Create new passing variable
dat_long <- 
  dat_long %>% 
  mutate_(.dots = setNames(list(
    interp(~ paste_(sprintf("p%02d", v), w),
           v = as.name(sett_vis$col_names$position),
           w = as.name("direction"))),
    "case")) %>% 
  select(-one_of(sett_vis$col_names$position, "direction"))

## Find cases to remove (no changes in visibility, only zeros)
## Remove cases from data
case_finder <-
  dat_long %>%
  group_by_("case") %>%
  summarise(mean = mean(percentage)) %>%
  filter(mean == 0 | mean == 100) %>%
  select_("case")

case_finder <- unname(unlist(case_finder))

dat_long <- dat_long %>% filter_(paste0("!", "case", "%in%", "case_finder"))

## Reshape to wide format
dat_long_wide <- 
  spread_(dat_long, 
          key = sett_vis$col_names$am,
          value = "percentage")

## Use case column as row names and remove case column
## Necessary for cluster algorithms
rownames(dat_long_wide) <- dat_long_wide[, "case"]
dat_long_wide[, "case"] <- NULL



# Clustering --------------------------------------------------------------

## Use clustering framework
dat_clust_results <-
  clust(dat_wide = dat_long_wide,
        k = sett_vis$cluster$k,
        procedure = sett_vis$cluster$proc,
        algorithm = sett_vis$cluster$algo,
        method4agglo = sett_vis$cluster$method_aggl,
        measure4diss = sett_vis$cluster$method_diss)

## Get group assignments
dat_clust_assignments <- dat_clust_results$assignment

# Convert passing variable to character for potential merging
#test1$case <- as.character(dat_clust_assignments$passing)
#dat_long$case <- as.character(dat_long$case)

## Join data and cluster assignment
dat_long_clustered <- 
  left_join(dat_long, 
            dat_clust_assignments, 
            by = setNames("id", "case")) %>% 
  mutate_(.dots = setNames(list(
    interp(~ as.factor(v),
           v = as.name("cluster_group"))),
    "cluster_group")) %>% 
  mutate(direction = ifelse(grepl("left", case), "left", "right"))



# Prepare data for Visualization ------------------------------------------

dat_long_clustered <- 
  dat_long_clustered %>% 
  mutate(direction = 
           factor(direction,
                  levels = c("left", "right"),
                  labels = c("Links", "Rechts")))

dat_long_clustered_summery <- 
  computeSummary(dat_long_clustered,
                 col_names_group = 
                   c(sett_vis$col_names$am,
                     "cluster_group"),
                     #"direction"),
                 col_names_values = "percentage")



# Vizualization -----------------------------------------------------------

plot_cluster <-
  ggplot() +
  geom_line(data = dat_long_clustered,
            aes_string(x = sett_vis$col_names$am,
                       y = "percentage",
                       group = "case",
                       colour = "cluster_group"),
            size = 0.5,
            alpha = 0.15) +
  geom_line(data = dat_long_clustered_summery,
            aes_string(x = sett_vis$col_names$am,
                       y = "mean",
                       color = "cluster_group"),
            size = 1) #+
  #facet_grid(as.formula(paste(".~", "direction")))

plot(plot_cluster)



# Postprocess -------------------------------------------------------------

plot_cluster_post <- 
  plot_cluster + 
  coord_cartesian(xlim = c(-50, 0),
                  ylim = c(0, 100)) +
  scale_x_continuous(expand = c(0, 0)) + 
  scale_color_brewer(palette = "Set1") + 
  ggtitle("Einsehbarkeit in Querstraßen") +
  labs(x = "Distanz bis zum Knotenpunkt (m)",
       y = "Einsehbarkeit (%)") +
  guides(color = F) + 
  theme_thesis()

plot(plot_cluster_post)




# Save plot ---------------------------------------------------------------

ggsave(filename = figureFileName("visibility"), 
       plot = plot_cluster_post,
       path = figurePath(),
       width = 8,
       height = 6,
       units = "cm",
       dpi = 1000,
       type = "cairo-png")

