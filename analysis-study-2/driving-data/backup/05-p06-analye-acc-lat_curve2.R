
# Settings ----------------------------------------------------------------

## Data
sett <- c()
sett$db$db_names$study2 <- "URBAN-MV-VIE_UniBw_Study-2"
sett$db$conn_names$study2 <- dbFindConnObj(sett$db$db_names$study2, output = F)
sett$db$src_names$dat <- "t_adtf_results_p06_dti_m_rnd1_acc_lat_max_aggr"

## Column names
sett$col_names$pxx <- "pxx"
sett$col_names$subject <- "subject_id"
sett$col_names$condition_speed <- "condition_speed"
sett$col_names$condition_vip <- "vip_group_code"

## Data base
sett$db$db_names$study2 <- "URBAN-MV-VIE_UniBw_Study-2"
sett$db$conn_names$study2 <- dbFindConnObj(sett$db$db_names$study2, output = F)

## Experimental conditions
sett$db$src_names$exp_cond_vip <- "t_experimental_conditions_vip"
sett$db$src_names$ds <- "t_q_ds_scores"

## Plot
sett$plot$file_name_prefix <- paste_(sprintf("p%02d", 6))



# Query data --------------------------------------------------------------

dat <- 
  dbGetSrc(sett$db$conn_names$study2, 
           sett$db$src_names$dat)

dat_exp_cond <- 
  dbGetSrc(sett$db$conn_names$study2, 
           sett$db$src_names$exp_cond_vip)

dat_ds <- 
  dbGetSrc(sett$db$conn_names$study2, 
           sett$db$src_names$ds)



# Preprocess data ---------------------------------------------------------

## Join data with experimental conditions
dat <- left_join(dat, dat_exp_cond)

## Join data with driving style
dat <- left_join(dat, dat_ds)

## Select measure
dat <- 
  dat %>% 
  select_(.dots = c(unlist(sett$col_names, use.names = FALSE), "mean")) %>% 
  mutate(condition_dir = "right") %>% 
  mutate()

## Create factors for plotting
dat$vip_group_code     <- 
  factor(dat$vip_group_code    ,
         levels = c("ED_low", "ED_high",
                    "US_low", "US_high"),
         labels = c("VIP-EA-N", "VIP-EA-H",
                    "VIP-US-N", "VIP-US-H"))

dat$condition_speed <-
  factor(dat$condition_speed,
         levels = c(50, 70),
         labels = c("50 km/h", "70 km/h"))



# Boxplot -----------------------------------------------------------------

plot_acc_lat_max_aggr <- 
  ggplot() +
  geom_violin(data = dat,
              aes(x = condition_speed,
                  y = mean,
                  fill = vip_group_code),
              size = 0.2,
              alpha = 0.25) +
  geom_boxplot(data = dat,
               aes(x = condition_speed,
                   y = mean,
                   fill = vip_group_code),
               alpha = 0.5,
               width = 0.2,
               size = 0.2,
               outlier.size = 0.2) +
  facet_grid(as.formula(
    paste(".", "~", 
          "vip_group_code"))) +
  coord_cartesian(ylim = c(0, 8))



# Postprocess -------------------------------------------------------------

plot_acc_lat_max_aggr_post <- 
  plot_acc_lat_max_aggr + 
  scale_fill_brewer(palette = "Set1") + 
  ggtitle(label = "Maximale laterale Beschleunigung in Kurve 2",
          subtitle = "Gruppierung nach VIP-Ausprägung") +
  labs(x = "Zielgeschwindigkeit",
       y = "Max lat. Beschleunigung (m/s²)") +
  guides(fill = F) + 
  theme_thesis()

sett$plot$file_name <- 
  paste_("acc", 
         sett$plot$file_name_prefix, 
         "max_acc_lat")

ggsave(filename = figureFileName(sett$plot$file_name), 
       plot = plot_acc_lat_max_aggr_post,
       path = figurePath(),
       width = 16,
       height = 5.5,
       units = "cm",
       dpi = 1000,
       type = "cairo-png")



# Inference ---------------------------------------------------------------

model_aov_ez <-
  aov_ez(id = sett$col_names$subject,
         dv = "mean",
         data = dat,
         within = sett$col_names$condition_speed,
         between = sett$col_names$condition_vip,
         print.formula = TRUE)

print(model_aov_ez)

model_aov_ez_post_hoc <-
  emmeans(model_aov_ez, ~ vip_group_code + condition_speed)

pairs(model_aov_ez_post_hoc, simple = "each")