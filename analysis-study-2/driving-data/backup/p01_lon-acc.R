
# Settings ----------------------------------------------------------------

## Data
sett <- c()
#sett$meta$df_name <- "dat_study2_t_adtf_pxx_full_aggr_dti_rnd1_intrpld_cut"
sett$meta$df_name <- "dat_study2_t_adtf_pxx_full_aggr_tti_rnd1_intrpld_cut"
sett$meta$am <- ifelse(grepl("dti", sett$meta$df_name), "dti", "tti")
sett$col_names$pxx <- "pxx"
sett$col_names$am <- findColNameForAM(sett$meta$am, get(sett$meta$df_name))
sett$col_names$dti <- findColNameForAM("dti", get(sett$meta$df_name))
sett$col_names$tti <- findColNameForAM("tti", get(sett$meta$df_name))
sett$col_names$measure_speed <- "speed_kmh"
sett$col_names$measure_acc_lon <- "acc_lon_ms2"
sett$col_names$group <- 
  c("passing", "pxx", "subject_id", 
    "round_id", "condition_speed", "vip_group_code")

## Filter
sett$filters$pxx <- 1
sett$filters$am <- "dti"
sett$filters$am_lower <- 0
sett$filters$am_upper <- 25

## Experimental conditions
sett$db$db_names$study2 <- "URBAN-MV-VIE_UniBw_Study-2"
sett$db$conn_names$study2 <- dbFindConnObj(sett$db$db_names$study2, output = F)
sett$db$src_names$exp_cond_vip <- "t_experimental_conditions_vip"
sett$db$src_names$ds <- "t_q_ds_scores"

## Processing
sett$proc$statistic <- "mean"
sett$col_names$measure_statistic <- 
  paste_(sett$col_names$measure,
         sett$proc$statistic)

## Save
sett$plot$labels$x <- ifelse(sett$meta$am == "dti", "Distanz (m)", "Zeit (s)")
sett$plot$labels$facets$speed <- c("50" = "50 km/h", "70" = "70 km/h")
sett$plot$file_name_prefix <- paste_(sprintf("p%02d", sett$filters$pxx))
sett$plot$file_name_suffix <- sett$meta$am



# Query data --------------------------------------------------------------

dat_exp_cond <- 
  dbGetSrc(sett$db$conn_names$study2, 
           sett$db$src_names$exp_cond_vip)

dat_ds <- 
  dbGetSrc(sett$db$conn_names$study2, 
           sett$db$src_names$ds)


# Preprocess data ---------------------------------------------------------

## Get data
dat <- get(sett$meta$df_name) 

## Join data with experimental conditions
dat <- 
  left_join(dat,
            dat_exp_cond)

## Join data with driving style
dat <- 
  left_join(dat,
            dat_ds) #%>% 
              #select(subject_id, ds_sc))

## Filter data
dat <- 
  dat %>% 
  ## Filter for position
  filter_(paste(sett$col_names$pxx, "==", sett$filters$pxx))

## Select data
dat <- 
  dat %>% 
  select("passing", "pxx", "subject_id", 
         "round_id", "condition_speed", 
         "vip_group_code", 
         starts_with("ds"),
         #sett$col_names$am,
         grep(paste("dti|tti"), names(dat), value = TRUE),
         sett$col_names$measure_speed,
         sett$col_names$measure_acc_lon,
         "speed_kmh")



# Compute summary for speed -----------------------------------------------

dat_speed_summary <- 
  computeSummary(dat,
                 col_names_group = 
                   c(sett$col_names$am,
                     "condition_speed"), 
                 col_names_values = sett$col_names$measure_speed)



# Prepare data for visualization ------------------------------------------

# dat_plot <- dat
# dat_plot$condition_speed <- 
#   factor(dat_plot$condition_speed,
#          levels = c(50, 70),
#          labels = c("50 km/h", "70 km/h"))
# 
# dat_speed_summary_plot <- dat_speed_summary
# dat_speed_summary_plot$condition_speed <- 
#   factor(dat_speed_summary_plot$condition_speed,
#          levels = c(50, 70),
#          labels = c("50 km/h", "70 km/h"))



# Visualize speed data ----------------------------------------------------

dat_speed_targets <- data.frame(
  condition_speed = unique(dat$condition_speed),
  speed = c(50, 70)
)

plot_speed <- 
  ggplot() +
  geom_line(data = dat,
            aes_string(x = sett$col_names$am,
                       y = sett$col_names$measure_speed,
                       group = "passing"),
            color = "grey75",
            size = 0.2) +
  facet_grid(~ condition_speed,
             labeller = 
               labeller(condition_speed = sett$plot$labels$facets$speed)) + 
  geom_hline(dat = dat_speed_targets,
             aes(yintercept = speed),
             size = 0.3,
             linetype = "dashed") +
  geom_line(data = dat_speed_summary,
            aes_string(x = sett$col_names$am,
                       y = "median",
                       group = "condition_speed"),
            size = 0.5,
            #color = RColorBrewer::brewer.pal(3, "Set1")[2])# + 
            color = convRGB2Hex(74, 84, 150)) +
  coord_cartesian(ylim = c(0, 100))

#plot(plot_speed)



# Post process ------------------------------------------------------------

temp_subtitle <- paste("Individuelle Geschwindigkeitsverläufe",
                       ifelse(sett$meta$am == "dti",
                              "(nach Distanz)",
                              "(nach Zeit)"),
                       "und Mittelwert")

plot_speed_post <- 
  plot_speed + 
  scale_y_continuous(breaks = seq(0, 100, 20)) +
  ggtitle(label = "Geschwindigkeitsverlauf beim Anfahren",
          subtitle = temp_subtitle) +
  labs(x = sett$plot$labels$x,
       y = "Geschwindigkeit (km/h)") +
  guides(color = F) +
  scale_color_brewer(palette = "Set1") +
  theme_thesis()

sett$plot$file_name <- 
  paste_(sett$plot$file_name_prefix,
         "speed",
         sett$plot$file_name_suffix)

ggsave(filename = figureFileName(sett$plot$file_name), 
       plot = plot_speed_post,
       path = figurePath(),
       width = 16,
       height = 5.5,
       units = "cm",
       dpi = 1000,
       type = "cairo-png")



# AM when reaching target -------------------------------------------------

dat_reached_speed_m10 <- 
  dat %>%
  group_by(passing, condition_speed) %>%
  filter(round(speed_kmh) >= 40) %>% 
  filter(row_number() == 1) %>%
  data.frame()

dat_reached_speed_m10_summary <- 
  computeSummary(dat_reached_speed_m10,
                 col_names_group = "condition_speed",
                 col_names_values = grep(paste("dti|tti"), names(dat), value = TRUE))





# Prepare data for long. acc. ---------------------------------------------

dat_acc <- 
  dat %>% 
  filter_(paste(sett$col_names$dti, "<= 150"))



# Compute summary for lon. acc. -------------------------------------------

dat_acc_summary <- 
  computeSummary(dat_acc,
                 col_names_group = 
                   c(sett$col_names$am,
                     "condition_speed"), 
                 col_names_values = sett$col_names$measure_acc_lon)



# Visualize acc data ------------------------------------------------------

plot_acc <- 
  ggplot() +
  geom_line(data = dat_acc,
            aes_string(x = sett$col_names$am,
                       y = sett$col_names$measure_acc_lon,
                       group = "passing"),
            color = "grey75",
            size = 0.2) +
  facet_grid(~ condition_speed,
             labeller = 
               labeller(condition_speed = sett$plot$labels$facets$speed)) +
  geom_line(data = dat_acc_summary,
            aes_string(x = sett$col_names$am,
                       y = "median",
                       group = "condition_speed"),
            size = 0.5,
            color = convRGB2Hex(74, 84, 150)) + 
  coord_cartesian(ylim = c(-2, 8)) + 
  scale_y_continuous(breaks = seq(-2, 10, 2))

#plot(plot_acc)



# Post process ------------------------------------------------------------

temp_subtitle <- paste("Individuelle Beschleunigungsverläufe",
                       ifelse(sett$meta$am == "dti",
                              "(nach Distanz)",
                              "(nach Zeit)"),
                       "und Mittelwert")

plot_acc_post <- 
  plot_acc + 
  ggtitle(label = "Longitudinale Beschleunigung beim Anfahren",
          subtitle = temp_subtitle) +
  labs(x = sett$plot$labels$x,
       y = "Beschleunigung (m/s²)") +
  guides(color = F) +
  scale_color_brewer(palette = "Set1") +
  theme_thesis()

sett$plot$file_name <- 
  paste_(sett$plot$file_name_prefix,
         "acc",
         sett$plot$file_name_suffix)

ggsave(filename = figureFileName(sett$plot$file_name), 
       plot = plot_acc_post,
       path = figurePath(),
       width = 16,
       height = 5.5,
       units = "cm",
       dpi = 1000,
       type = "cairo-png")




# Compute statistics ------------------------------------------------------

## Max. lon. acc.
dat_acc_lon_max <- 
  dat_acc %>% 
  group_by_(sett$col_names$group) %>% 
  filter_(paste(sett$col_names$measure_acc_lon, "==",
                  "max(", sett$col_names$measure_acc_lon, ")")) %>% 
  filter(row_number() == 1) %>% 
  data.frame()

## Min. lon. acc.
dat_acc_lon_min <- 
  dat %>% 
  group_by_(sett$col_names$group) %>% 
  filter_(paste(sett$col_names$measure_acc_lon, "==",
                "min(", sett$col_names$measure_acc_lon, ")")) %>% 
  data.frame()


## Mean. lon. acc.
dat_acc_lon_mean <- 
  dat %>% 
  #group_by_(sett$col_names$group) %>% 
  group_by_(.dots = c(sett$col_names$group,
                      grep("ds", names(dat), value=TRUE))) %>%
  summarize_(.dots = setNames(list(
    interp(~ mean(var),
           var = as.name(sett$col_names$measure_acc_lon) )),
    sett$col_names$measure_acc_lon)) %>% 
  data.frame()



# Write statistics to database --------------------------------------------

dbWriteTable(get(sett$db$conn_names$study2),
             "t_adtf_results_p01_acc_lon_max",
             dat_acc_lon_max,
             row.names = FALSE,
             overwrite = T)


dbWriteTable(get(sett$db$conn_names$study2),
             "t_adtf_results_p01_acc_lon_min",
             dat_acc_lon_min,
             row.names = FALSE,
             overwrite = T)

dbWriteTable(get(sett$db$conn_names$study2),
             "t_adtf_results_p01_acc_lon_mean",
             dat_acc_lon_mean,
             row.names = FALSE,
             overwrite = T)



# Play viz ----------------------------------------------------------------

ggplot() + 
  geom_density(data = dat_acc_lon_max,
               aes(acc_lon_ms2)) + 
  facet_grid(.~condition_speed)

test2_dat50 <- rnorm(length(test_dat50),
                     mean(test_dat50$acc_lon_ms2),
                     sd(test_dat50$acc_lon_ms2))

test2_dat70 <- rnorm(length(test_dat70),
                     mean(test_dat70$acc_lon_ms2),
                     sd(test_dat70$acc_lon_ms2))


test_dat50 <- dat_acc_lon_max %>% filter(condition_speed == 50)
test_dat70 <- dat_acc_lon_max %>% filter(condition_speed == 70)
test_dat50_ecdf <- computeECDF(test_dat50$acc_lon_ms2)
test_dat70_ecdf <- computeECDF(test_dat70$acc_lon_ms2)

test2_dat50_ecdf <- computeECDF(test2_dat50)
test2_dat70_ecdf <- computeECDF(test2_dat70)


test_dat_ecdf <- 
  rbind(data.frame(test_dat50_ecdf, condition_speed = "50 km/h"),
        data.frame(test_dat70_ecdf, condition_speed = "70 km/h"))

test2_dat_ecdf <- 
  rbind(data.frame(test2_dat50_ecdf, condition_speed = "50 km/h"),
        data.frame(test2_dat70_ecdf, condition_speed = "70 km/h"))

plot_ecdf <- 
  ggplot() + 
  # geom_ribbon(data = test_dat_ecdf,
  #             aes(x,
  #                 ymin = ci_lower,
  #                 ymax = ci_upper,
  #                 fill = condition_speed),
  #             size = 0.5,
  #             alpha = 0.15) +
  geom_line(data = test_dat_ecdf,
            aes(x,
                y,
                color = condition_speed),
            size = 0.5) +
  geom_line(data = test2_dat_ecdf,
            aes(x,
                y,
                color = condition_speed),
            size = 0.5,
            linetype = "dashed") +
  coord_cartesian(xlim = c(0, 8))#+
  #geom_hline(aes(yintercept = c(0.25, 0.5, 0.75)))


plot_ecdf_post <- 
  plot_ecdf + 
  scale_color_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1") +
  guides(color = guide_legend(title = "Zielgeschwindigkeit"),
         fill = guide_legend(title = "Zielgeschwindigkeit")) +
  ggtitle("Longitudinale Beschleunigung beim Anfahren",
          subtitle = "Verteilung maximaler longitudinaler Beschleunigung in v50 und v70") + 
  labs(x = "Longitudinale Beschleunigung (m/s²)",
       y = "Wahrscheinlichkeit") +
  theme_thesis() + 
  theme(legend.justification=c(-0.005,1.005), 
        legend.position=c(0,1),
        legend.key.height = unit(0.25, "cm"),
        legend.key.width = unit(0.5, "cm"))

plot_ecdf_post

sett$plot$file_name <- 
  paste_(sett$plot$file_name_prefix,
         "acc_v50_v70_ecdf",
         sett$plot$file_name_suffix)

ggsave(filename = figureFileName(sett$plot$file_name), 
       plot = plot_ecdf_post,
       path = figurePath(),
       width = 16,
       height = 5.5,
       units = "cm",
       dpi = 1000,
       type = "cairo-png")

  
  

# Grouping ----------------------------------------------------------------

dat_acc_lon_max_test <- 
  dat_acc_lon_max %>% 
  mutate(a_model = ifelse(acc_lon_ms2 <= 2.75,
                          1,
                          ifelse(acc_lon_ms2 >2.75 & acc_lon_ms2 <= 3.75,
                                 2, 3)))

ggplot() + 
  geom_bar(data = dat_acc_lon_max_test,
           aes(a_model)) + 
  facet_grid(.~round_id)


# fdfd --------------------------------------------------------------------

ggplot() + 
  geom_boxplot(data = dat_acc_lon_max,
               aes(x = vip_group_code,
                   y = acc_lon_ms2)) + 
  facet_grid(.~condition_speed)

ggplot() + 
  geom_boxplot(data = dat_acc_lon_min,
               aes(x = vip_group_code,
                   y = acc_lon_ms2)) + 
  facet_grid(.~condition_speed)

ggplot() + 
  geom_boxplot(data = dat_acc_lon_mean,
               aes(x = vip_group_code,
                   y = acc_lon_ms2)) + 
  facet_grid(.~condition_speed)



temp <- c("round_id", "vip_group_code")
dat_acc_lon_max_summary <-
  computeSummary(dat_acc_lon_min, 
                 col_names_group = temp,
                 col_names_values = sett$col_names$am)

dat_acc_lon_max_summary_vip <-
  computeSummary(dat_acc_lon_max, 
                 col_names_group = "vip_group_code",
                 col_names_values = sett$col_names$am)

test <-
  computeSummary(dat,
                 col_names_group = c(sett$col_names$am, "vip_group_code", "round_id"), 
                 col_names_values = sett$col_names$measure)

ggplot() + 
  geom_line(data = test,
            aes_string(x = sett$col_names$am ,
                       y = "mean",
                       color = "vip_group_code")) + 
  scale_color_brewer(palette = "Set1") + 
  facet_grid(round_id ~ .)
  # geom_line(data = test,
  #           aes_string(x = sett$col_names$dti,
  #                      y = "max",
  #                      color = "vip_group_code")) + 
  # geom_line(data = test,
  #           aes_string(x = sett$col_names$dti,
  #                      y = "mean",
  #                      color = "vip_group_code")) + 
  # geom_line(data = test,
  #           aes_string(x = sett$col_names$dti,
  #                      y = "median",
  #                      color = "vip_group_code"))


# test --------------------------------------------------------------------

rm(dat_test)
#dat_test <- dat %>% filter(round_id == "t2_v50")
dat_test <- dat %>% group_by(subject_id, condition_speed, dti_m_rnd1) %>%
  summarize(acc_lon_ms2 = mean(acc_lon_ms2 ))
dat_test <- dat %>% filter(condition_speed == 70)
test_p <- c()
for (i in unique(dat_test$dti_m_rnd1)) {
  print(i)
  row_finder <- dat_test$dti_m_rnd1 == i
  temp_dat <- dat_test[row_finder, ]
  row_finder <- grepl("US_high", temp_dat$vip_group_code)
  temp_dat_group1 <- temp_dat[row_finder, ]
  row_finder <- grepl("US_low", temp_dat$vip_group_code)
  temp_dat_group2 <- temp_dat[row_finder, ]
  temp_p <- t.test(temp_dat_group1$acc_lon_ms2, 
                   temp_dat_group2$acc_lon_ms2)
  test_p <- c(test_p, temp_p$p.value)
}

plot(unique(dat_test$dti_m_rnd1), test_p, type = "l")
abline(a = 0.05, b = 0, col = "red")



# fdfsdf ------------------------------------------------------------------



test_ecdf <- computeECDF(test$sd)

ggplot() + 
  geom_line(data = test_ecdf,
            aes(x = x,
                y = y)) + 
  geom_ribbon(data = test_ecdf,
              aes(x = x,
                  ymin = ci_lower,
                  ymax = ci_upper),
              alpha = 0.25)


# Visualize DTI of max. lon. acc. -----------------------------------------

plot_dti_lon_acc_max <- 
  ggplot() + 
  geom_violin(data = dat_acc_lon_max,
              (aes_string(x = "1",
                          y = sett$col_names$dti))) + 
  coord_flip()

plot(plot_dti_lon_acc_max)

test <- computeECDF(dat_acc_lon_max$dti_m_rnd1)


# fsdfdsf -----------------------------------------------------------------



## Filter for distance
filter_(paste(sett$col_names$dti, "<=", sett$filters$dti_upper)) %>% 
  ## Filter for positive values only
  filter_(paste(sett$col_names$measure, ">= 0"))



# Extract statistics ------------------------------------------------------

## Compute statistics
dat_filtered_summary <- 
  computeSummary(dat_filtered, 
                 col_names_group = c(sett$col_names$group, "round_id"),
                 col_names_values = sett$col_names$measure)

## Compute ECDF
dat_filtered_summary <- 
  dat_filtered_summary %>% 
  group_by(round_id) %>% 
  arrange_(sett$proc$statistic) %>% 
  mutate_(.dots =
            setNames(list(
              interp(~ computeECDF(var)$y,
                     var = as.name(sett$proc$statistic))),
              "ecdf_prob"))



# Visualize statistics ----------------------------------------------------

plot_ecdf <- 
  ggplot() + 
  geom_step(data = dat_filtered_summary,
            aes_string(x = sett$proc$statistic ,
                       y = "ecdf_prob",
                       group = "round_id",
                       color = "round_id"))

plot(plot_ecdf)

plot_boxplot <- 
  ggplot() + 
  geom_violin(data = dat_filtered_summary,
              aes_string(x = "round_id",
                         y = sett$proc$statistic)) + 
  geom_boxplot(data = dat_filtered_summary,
               aes_string(x = "round_id",
                          y = sett$proc$statistic))

plot(plot_boxplot)              