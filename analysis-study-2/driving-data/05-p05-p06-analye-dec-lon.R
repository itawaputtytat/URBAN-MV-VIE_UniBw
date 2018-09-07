
# Settings ----------------------------------------------------------------

## Data
sett <- c()
sett$db$db_names$study2 <- "URBAN-MV-VIE_UniBw_Study-2"
sett$db$conn_names$study2 <- dbFindConnObj(sett$db$db_names$study2, output = F)
sett$db$src_names$dat_p05 <- "t_adtf_results_p05_dti_m_rnd1_dec_lon_max_aggr"
sett$db$src_names$dat_p06 <- "t_adtf_results_p06_dti_m_rnd1_dec_lon_max_aggr"

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
sett$plot$file_name_prefix <- paste(sprintf("p%02d", c(5, 6)), collapse = "_")



# Query data --------------------------------------------------------------

dat_p05 <- 
  dbGetSrc(sett$db$conn_names$study2, 
           sett$db$src_names$dat_p05)

dat_p06 <- 
  dbGetSrc(sett$db$conn_names$study2, 
           sett$db$src_names$dat_p06)

dat_exp_cond <- 
  dbGetSrc(sett$db$conn_names$study2, 
           sett$db$src_names$exp_cond_vip)

dat_ds <- 
  dbGetSrc(sett$db$conn_names$study2, 
           sett$db$src_names$ds)



# Preprocess data ---------------------------------------------------------

## Join data with experimental conditions
dat_p05 <- left_join(dat_p05, dat_exp_cond)
dat_p06 <- left_join(dat_p06, dat_exp_cond)

## Join data with driving style
dat_p05 <- left_join(dat_p05, dat_ds)
dat_p06 <- left_join(dat_p06, dat_ds)

## Select measure
dat_p05 <- 
  dat_p05 %>% 
  select_(.dots = c(unlist(sett$col_names, use.names = FALSE), "mean")) %>% 
  mutate(condition_curve = "curve1")

dat_p06 <- 
  dat_p06 %>% 
  select_(.dots = c(unlist(sett$col_names, use.names = FALSE), "mean")) %>% 
  mutate(condition_curve = "curve2")

## Combine both turning directions
dat_both <- rbind(dat_p05, dat_p06)

## Create factors for plotting
dat_both$vip_group_code     <- 
  factor(dat_both$vip_group_code    ,
         levels = c("ED_low", "ED_high",
                    "US_low", "US_high"),
         labels = c("VIP-EA-N", "VIP-EA-H",
                    "VIP-US-N", "VIP-US-H"))

dat_both$condition_speed_curve <- 
  paste(dat_both$condition_speed,
        dat_both$condition_curve)

dat_both$condition_speed_curve <-
  factor(dat_both$condition_speed_curve,
         levels = c("50 curve1", "50 curve2",
                    "70 curve1", "70 curve2"),
         labels = c("v50-K1", "v50-K2",
                    "v70-K1", "v70-K2"))



# Boxplot -----------------------------------------------------------------

plot_acc_lon_max_aggr <- 
  ggplot() +
  geom_violin(data = dat_both,
              aes(x = condition_speed_curve,
                  y = mean,
                  fill = vip_group_code),
              size = 0.2,
              alpha = 0.25) +
  geom_boxplot(data = dat_both,
               aes(x = condition_speed_curve,
                   y = mean,
                   fill = vip_group_code),
               alpha = 0.5,
               width = 0.2,
               size = 0.2,
               outlier.size = 0.2) +
  facet_grid(as.formula(
    paste(".", "~", 
          "vip_group_code"))) +
  coord_cartesian(ylim = c(-8, 0))



# Postprocess -------------------------------------------------------------

plot_acc_lon_max_aggr_post <- 
  plot_acc_lon_max_aggr + 
  scale_fill_brewer(palette = "Set1") + 
  ggtitle(label = "Maximale longitudinale Verzögerung vor Kurven",
          subtitle = "Gruppierung nach VIP-Ausprägung") +
  labs(x = "Zielgeschwindigkeit und Kurve",
       y = "Max lon. Beschleunigung (m/s²)") +
  guides(fill = F) + 
  theme_thesis()

sett$plot$file_name <- 
  paste_("acc", 
         sett$plot$file_name_prefix, 
         "max_dec_lon")

ggsave(filename = figureFileName(sett$plot$file_name), 
       plot = plot_acc_lon_max_aggr_post,
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
         data = dat_both,
         within = c(sett$col_names$condition_speed,
                    "condition_dir"),
         between = sett$col_names$condition_vip,
         print.formula = TRUE)

print(model_aov_ez)

model_aov_ez_post_hoc <-
  emmeans(model_aov_ez, ~ vip_group_code + condition_speed + condition_curve)

pairs(model_aov_ez_post_hoc, simple = "each")