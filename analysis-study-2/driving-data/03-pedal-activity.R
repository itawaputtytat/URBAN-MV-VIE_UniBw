
# Settings ----------------------------------------------------------------

## Data
sett <- c()
sett$meta$df_name <- "dat_study2_t_adtf_pxx_full_aggr_dti_rnd1_intrpld_cut"
#sett$meta$df_name <- "dat_study2_t_adtf_pxx_full_aggr_tti_rnd1_intrpld_cut"
sett$meta$am <- ifelse(grepl("dti", sett$meta$df_name), "dti", "tti")
#sett$meta$am <- "tta_s"

## Filter
sett$filters$pxx <- 4
sett$filters$am <- "dti"
sett$filters$am_lower <- 0
sett$filters$am_upper <- 25

## Column names
sett$col_names$pxx <- "pxx"
sett$col_names$am <- findColNameForAM(sett$meta$am, get(sett$meta$df_name))
sett$col_names$am_start <- paste_(sett$col_names$am, "start")
sett$col_names$am_end <- paste_(sett$col_names$am, "end")
sett$col_names$dti <- findColNameForAM("dti", get(sett$meta$df_name))
sett$col_names$tti <- findColNameForAM("tti", get(sett$meta$df_name))
sett$col_names$measure_speed <- "speed_kmh"
sett$col_names$measure_acc_lon <- "acc_lon_ms2"
sett$col_names$id <- "passing"
sett$col_names$group <- 
  c("passing", "pxx", "subject_id", 
    "round_id", "condition_speed", "condition_run",
    "vip_group_code")

## Experimental conditions
sett$db$db_names$study2 <- "URBAN-MV-VIE_UniBw_Study-2"
sett$db$conn_names$study2 <- dbFindConnObj(sett$db$db_names$study2, output = F)
sett$db$src_names$exp_cond_vip <- "t_experimental_conditions_vip"
sett$db$src_names$ds <- "t_q_ds_scores"

## Processing
sett$tresholds$crit_arr_means <- 0
sett$tresholds$crit_arr_means_before_braking <- 0

sett$proc$statistic <- "mean"
sett$col_names$measure_statistic <- 
  paste_(sett$col_names$measure,
         sett$proc$statistic)

## Plot
sett$plot$title_maneuver <- 
  ifelse(sett$filters$pxx == 2,
         "Rechtsabbiegen",
         "Linksabbiegen")
sett$plot$pedal$xlim_min <- -150
sett$plot$pedal$xlim_max <- 25
sett$plot$pedal$fill <- c("red3", "white", "green3")
sett$plot$file_name_prefix <- paste_(sprintf("p%02d", sett$filters$pxx))
sett$plot$file_name_suffix <- sett$meta$am
sett$plot$file_name_suffix_dir <- 
  ifelse(sett$filters$pxx == 2,
         "right",
         "left")

sett$plot$labels$x <- ifelse(sett$meta$am == "dti", "Distanz (m)", "Zeit (s)")
# sett$plot$labels$facets$speed <- c("50" = "50 km/h", "70" = "70 km/h")
sett$plot$labels$facet$run <- c("T1", "T2")
names(sett$plot$labels$facet$run) <- c(1, 2)

sett$plot$labels$facet$speed <- c("50 km/h", "70 km/h")
names(sett$plot$labels$facet$speed) <- c(50, 70)



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

## Correct acceleration pedal position
correctAccPedalPos(dat)

## Code pedal activity
codePedalActivity(dat, colname_acc_pedal_pos = "acc_pedal_position_perc_corr")

## Join data with experimental conditions
dat <- left_join(dat, dat_exp_cond)

## Join data with driving style
dat <- left_join(dat, dat_ds)

## Filter data
dat <- 
  dat %>% 
  ## Filter for position
  filter_(paste(sett$col_names$pxx, "==", sett$filters$pxx))



# Compute pedal activity measures -----------------------------------------

dat <-
  dat %>%
  filter_(paste(sett$col_names$am, "<=", 10))

dat_pedal_measures <- 
  computePedActMeasures(
    dat,
    col_name_id = sett$col_names$id,
    col_names_group = sett$col_names$group,
    col_names_measure = 
      c(sett$col_names$measure_speed,
        sett$col_names$measure_acc_lon), 
    col_name_am = sett$col_names$am,
    col_name_am_start = sett$col_names$am_start,
    col_name_am_end = sett$col_names$am_end,
    treshold = sett$tresholds$crit_arr_means,
    threhshold_before_braking = sett$tresholds$crit_arr_means_before_braking
  )



# Visualize releasing acc. pedal ------------------------------------------

plot_pedal_acc_t1_v50 <- 
  plotPedActSeq(dat_pedal_measures$dat_acc_t1 %>% 
                  filter(condition_speed == 50),
                col_name_am = sett$col_names$am,
                col_name_id = "passing",
                col_name_facet_row = "condition_run",
                facet_labeller = 
                  labeller(condition_run = sett$plot$labels$facet$run),
                sett_plot = sett$plot$pedal)

windows(); plot(plot_pedal_acc_t1_v50)

plot_pedal_acc_t1_v70 <- 
  plotPedActSeq(dat_pedal_measures$dat_acc_t1 %>% 
                  filter(condition_speed == 70),
                col_name_am = sett$col_names$am,
                col_name_id = "passing",
                col_name_facet_row = "condition_run",
                facet_labeller = 
                  labeller(condition_run = sett$plot$labels$facet$run),
                sett_plot = sett$plot$pedal)

windows(); plot(plot_pedal_acc_t1_v70)



# Postprocess -------------------------------------------------------------

sett$plot$title <- 
  paste("Lösen des Fahrpedals vor",
        sett$plot$title_maneuver)

plot_pedal_acc_t1_v50_post <- 
  plot_pedal_acc_t1_v50 + 
  ggtitle(label = sett$plot$title,
          subtitle = "Zielgeschwindigkeit: 50 km/h") +
  labs(x = sett$plot$labels$x,
       y = "Individuelle Fahrten") +
  guides(color = F) +
  scale_color_brewer(palette = "Set1") +
  theme_thesis() +
  theme(strip.text.y = element_text(angle=0))

sett$plot$file_name <- 
  paste_("pedal_act", 
         sprintf("p%s02", sett$filters$pxx), 
         #"v50_acc_release_right")
         "v50_acc_release_left")

ggsave(filename = figureFileName(sett$plot$file_name), 
       plot = plot_pedal_acc_t1_v50_post,
       path = figurePath(),
       width = 8,
       height = 6,
       units = "cm",
       dpi = 1000,
       type = "cairo-png")

plot_pedal_acc_t1_v70_post <- 
  plot_pedal_acc_t1_v70 + 
  ggtitle(label = sett$plot$title,
          subtitle = "Zielgeschwindigkeit: 50 km/h") +
  labs(x = sett$plot$labels$x,
       y = "Individuelle Fahrten") +
  guides(color = F) +
  scale_color_brewer(palette = "Set1") +
  theme_thesis() +
  theme(strip.text.y = element_text(angle=0))

sett$plot$file_name <- 
  paste_("pedal_act", 
         sprintf("p%s02", sett$filters$pxx), 
         sett$plot$file_name_suffix,
         "v70_acc_release",
         sett$plot$file_name_suffix_dir)

ggsave(filename = figureFileName(sett$plot$file_name), 
       plot = plot_pedal_acc_t1_v70_post,
       path = figurePath(),
       width = 8,
       height = 6,
       units = "cm",
       dpi = 1000,
       type = "cairo-png")



# Visualize first braking -------------------------------------------------

plot_pedal_brake_1st_v50 <- 
  plotPedActSeq(dat_pedal_measures$dat_brake_1st %>% 
                  filter(condition_speed == 50),
                col_name_am = sett$col_names$am,
                col_name_id = "passing",
                col_name_facet_row = "condition_run",
                facet_labeller = 
                  labeller(condition_run = sett$plot$labels$facet$run),
                sett_plot = sett$plot$pedal)

windows(); plot(plot_pedal_brake_1st_v50)

plot_pedal_brake_1st_v70 <- 
  plotPedActSeq(dat_pedal_measures$dat_brake_1st %>% 
                  filter(condition_speed == 70),
                col_name_am = sett$col_names$am,
                col_name_id = "passing",
                col_name_facet_row = "condition_run",
                facet_labeller = 
                  labeller(condition_run = sett$plot$labels$facet$run),
                sett_plot = sett$plot$pedal)

windows(); plot(plot_pedal_brake_1st_v70)



# Postprocess -------------------------------------------------------------

sett$plot$title <- 
  paste("Betätigen des Bremspedals vor",
        sett$plot$title_maneuver)

plot_pedal_brake_1st_v50_post <- 
  plot_pedal_brake_1st_v50 + 
  ggtitle(label = sett$plot$title,
          subtitle = "Zielgeschwindigkeit: 50 km/h") +
  labs(x = sett$plot$labels$x,
       y = "Individuelle Fahrten") +
  guides(color = F) +
  scale_color_brewer(palette = "Set1") +
  theme_thesis() +
  theme(strip.text.y = element_text(angle=0))

sett$plot$file_name <- 
  paste_("pedal_act", 
         sprintf("p%02d", sett$filters$pxx), 
         #"v50_brake_right")
         "v50_brake_left")

ggsave(filename = figureFileName(sett$plot$file_name), 
       plot = plot_pedal_brake_1st_v50_post,
       path = figurePath(),
       width = 8,
       height = 6,
       units = "cm",
       dpi = 1000,
       type = "cairo-png")

plot_pedal_brake_1st_v70_post <- 
  plot_pedal_brake_1st_v70 + 
  ggtitle(label = sett$plot$title,
          subtitle = "Zielgeschwindigkeit: 70 km/h") +
  labs(x = sett$plot$labels$x,
       y = "Individuelle Fahrten") +
  guides(color = F) +
  scale_color_brewer(palette = "Set1") +
  theme_thesis() +
  theme(strip.text.y = element_text(angle=0))

sett$plot$file_name <- 
  paste_("pedal_act", 
         sprintf("p%02d", sett$filters$pxx), 
         sett$plot$file_name_suffix,
         "v70_brake",
         sett$plot$file_name_suffix_dir)

ggsave(filename = figureFileName(sett$plot$file_name), 
       plot = plot_pedal_brake_1st_v70_post,
       path = figurePath(),
       width = 8,
       height = 6,
       units = "cm",
       dpi = 1000,
       type = "cairo-png")



# Evolution of releasing acceleration pedal -------------------------------

dat_reduced_acc_evo <- 
  computePedActEvo(
    dat = dat_pedal_measures$dat_reduced_acc_t1,
    dat_template = dat,
    col_name_id = "passing",
    col_names_group = c("round_id", "condition_speed", "condition_run"),
    col_name_am = sett$col_names$am,
    col_name_value = sett$col_names$am_end)

dat_reduced_acc_evo$round_id <- 
  factor(dat_reduced_acc_evo$round_id,
         levels = c("t1_v50",
                    "t2_v50",
                    "t1_v70",
                    "t2_v70"),
         labels = c("50 km/h (T1)",
                    "50 km/h (T2)",
                    "70 km/h (T1)",
                    "70 km/h (T2)"))

plot_pedal_acc_evo <- 
  ggplot() +
  #plot_pedal_acc_t1_evo +
  geom_line(data = dat_reduced_acc_evo,
            aes_string(x = sett$col_names$am,
                       y = "percentage",
                       color = "factor(round_id)",
                       group = "factor(round_id)"),
            size = 0.25,
            alpha = 0.25) +
  geom_line(data = dat_reduced_acc_evo,
            aes_string(x = sett$col_names$am,
                       y = "percentage",
                       color = "factor(round_id)",
                       linetype = "factor(round_id)"),
            size = 0.25)

plot(plot_pedal_acc_evo)



# Postprocess -------------------------------------------------------------

sett$plot$title <- "Verteilung der Fahrpedal-Lösen-Aktivität"
sett$plot$subtitle <- 
  paste("Abbiegerichtung:", 
        sub("abbiegen", "", sett$plot$title_maneuver))

sett$plot$y <- "Anteil an Fahrten (%)"

plot_pedal_acc_evo_post <- 
  plot_pedal_acc_evo + 
  ggtitle(label = sett$plot$title,
          subtitle = sett$plot$subtitle) + 
  labs(x = sett$plot$labels$x,
       y = sett$plot$y) + 
  scale_linetype_manual("Bedingung", values = c("solid", "dashed", "solid", "dashed")) +
  scale_color_manual("Bedingung", 
                     values = c(
                       brewer.pal(9, "Set1")[1],
                       brewer.pal(9, "Set1")[1],
                       brewer.pal(9, "Set1")[2],
                       brewer.pal(9, "Set1")[2]
                     )) +
  theme_thesis() + 
  theme(strip.text.y = element_text(angle=0)) + 
  theme(legend.justification=c(-0.005,1.005),
        legend.position=c(0,1),
        legend.key.height = unit(0.25, "cm"),
        legend.key.width = unit(0.5, "cm"),
        legend.margin = margin(0.1, 0.1, 0.1, 0.1, "cm"))

windows(); plot(plot_pedal_acc_evo_post)

sett$plot$file_name <- 
  paste_("pedal_act", 
         sprintf("p%02d", sett$filters$pxx), 
         sett$plot$file_name_suffix,
         "evo_acc_release",
         sett$plot$file_name_suffix_dir)

ggsave(filename = figureFileName(sett$plot$file_name), 
       plot = plot_pedal_acc_evo_post,
       path = figurePath(),
       width = 8,
       height = 5.5,
       units = "cm",
       dpi = 1000,
       type = "cairo-png")



# Evolution of brake pedal activity ---------------------------------------

dat_reduced_brake_1st_evo <- 
  computePedActEvo(
    dat = dat_pedal_measures$dat_reduced_brake_1st,
    dat_template = dat,
    col_name_id = "passing",
    col_names_group = c("round_id", "condition_speed", "condition_run"),
    col_name_am = sett$col_names$am,
    col_name_value = sett$col_names$am_start)

dat_reduced_brake_1st_evo$round_id <- 
  factor(dat_reduced_brake_1st_evo$round_id,
         levels = c("t1_v50",
                    "t2_v50",
                    "t1_v70",
                    "t2_v70"),
         labels = c("50 km/h (T1)",
                    "50 km/h (T2)",
                    "70 km/h (T1)",
                    "70 km/h (T2)"))

plot_pedal_brake_1st_evo <- 
  ggplot() + 
  geom_line(data = dat_reduced_brake_1st_evo,
            aes_string(x = sett$col_names$am,
                       y = "percentage",
                       color = "factor(round_id)",
                       group = "factor(round_id)"),
            size = 0.25,
            alpha = 0.25) +
  geom_line(data = dat_reduced_brake_1st_evo,
            aes_string(x = sett$col_names$am,
                       y = "percentage",
                       color = "factor(round_id)",
                       linetype = "factor(round_id)"),
            size = 0.25) 

plot(plot_pedal_brake_1st_evo)



# Postprocess -------------------------------------------------------------

sett$plot$title <- "Verteilung der Bremspedal-Aktivität"
sett$plot$subtitle <- 
  paste("Abbiegerichtung:", 
        sub("abbiegen", "", sett$plot$title_maneuver))

sett$plot$y <- "Anteil an Fahrten (%)"

plot_pedal_brake_1st_evo_post <- 
  plot_pedal_brake_1st_evo + 
  ggtitle(label = sett$plot$title,
          subtitle = sett$plot$subtitle) + 
  labs(x = sett$plot$labels$x,
       y = sett$plot$y) + 
  scale_linetype_manual("Bedingung", values = c("solid", "dashed", "solid", "dashed")) +
  scale_color_manual("Bedingung", 
                     values = c(
                       brewer.pal(9, "Set1")[1],
                       brewer.pal(9, "Set1")[1],
                       brewer.pal(9, "Set1")[2],
                       brewer.pal(9, "Set1")[2]
                     )) +
  theme_thesis() + 
  theme(strip.text.y = element_text(angle=0)) + 
  theme(legend.justification=c(-0.005,1.005),
        legend.position=c(0,1),
        legend.key.height = unit(0.25, "cm"),
        legend.key.width = unit(0.5, "cm"),
        legend.margin = margin(0.1, 0.1, 0.1, 0.1, "cm"))

#windows(); plot(plot_pedal_brake_1st_evo_post)

sett$plot$file_name <- 
  paste_("pedal_act", 
         sprintf("p%02d", sett$filters$pxx), 
         sett$plot$file_name_suffix,
         "evo_brake",
         sett$plot$file_name_suffix_dir)

ggsave(filename = figureFileName(sett$plot$file_name), 
       plot = plot_pedal_brake_1st_evo_post,
       path = figurePath(),
       width = 8,
       height = 5.5,
       units = "cm",
       dpi = 1000,
       type = "cairo-png")

