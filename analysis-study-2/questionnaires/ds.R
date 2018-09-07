
# Settings ----------------------------------------------------------------

sett <- list()
sett$db$db_names$study2 <- "URBAN-MV-VIE_UniBw_Study-2"
sett$db$db_names$quest <- "Questionnaires"
sett$db$conn_names$study2 <- dbFindConnObj(sett$db$db_names$study2, output = F)
sett$db$conn_names$quest <- dbFindConnObj(sett$db$db_names$quest, output = F)
sett$db$src_names$ds <- "v_q_driving_style"

initQuestSettings("ds", 
                    sett$db$conn_names$quest, 
                    "Driving-style_Arndt_2011_adapted",
                    lang = "ger")



# Query data --------------------------------------------------------------

dat_ds <- 
  dbGetSrc(sett$db$conn_names$study2,
           sett$db$src_names$ds)



# Check for missing values ------------------------------------------------

md.pattern(dat_ds)



# Preprocess data ---------------------------------------------------------

dat_ds_inv <-
  recodeItems(dat_ds,
              sett$quest$ds$items$col_names_to_reverse,
              sett$quest$ds$scale$values_min,
              sett$quest$ds$scale$values_max)

dat_ds_inv_long <- 
  reshapeLong(dat_ds_inv, col_names_to_keep = "subject_id")

dat_ds_inv_long_summary <- 
  computeSummary(dat_ds_inv_long)

dat_ds_inv_scores <- 
  computeScores(dat_ds_inv,
                sett$quest$ds$subscales$items)

dat_ds_inv_scores_long <- 
  reshapeLong(dat_ds_inv_scores, "subscale", "value", "subject_id")



# Save data ---------------------------------------------------------------

dbWriteTable(get(sett$db$conn_names$study2),
             "t_q_ds_scores",
             dat_ds_inv_scores,
             row.names = F,
             overwrite = T)



# Prepare data for subscale plot ------------------------------------------

dat_ds_inv_scores_long <- 
  convertSubscaleToFactor(dat_ds_inv_scores_long,
                          sett$quest$ds$subscales$col_names_unique,
                          sett$quest$ds$subscales$names_lang,
                          sett$quest$ds$meta$lang)



# Visualize scores --------------------------------------------------------

plot_subscales <- 
  ggplot() + 
  # geom_violin(data = dat_ds_inv_scores_long,
  #             aes(x = subscale_fct,
  #                 y = value, 
  #                 fill = subscale_fct,
  #                 color = subscale_fct),
  #             alpha = 0.1,
  #             size = 0.2) +
  geom_boxplot(data = dat_ds_inv_scores_long,
               aes(x = subscale_fct,
                   y = value,
                   fill = subscale_fct),
               alpha = 0.5,
               #width = 0.1,
               fatten = 3,
               size = 0.2,
               outlier.size = 0.5,
               outlier.alpha = 1) +
  coord_flip(ylim = c(1,5))

plot(plot_subscales)


# Post process subscales plot ---------------------------------------------

plot_subscales_post <- 
  plot_subscales + 
  
  ## Scale
  scale_x_discrete(position = "top") +
  scale_fill_manual(values = rep(convRGB2Hex(47, 84, 150, 1), 11)) + 
  scale_color_manual(values = rep(convRGB2Hex(47, 84, 150, 1), 11)) +
  # scale_fill_manual(values = rev(RColorBrewer::brewer.pal(5, "Set1")[1:5])) +
  # scale_color_manual(values = rev(RColorBrewer::brewer.pal(5, "Set1")[1:5])) +
  
  ## Label
  ggtitle(label = "Fahrstil",
          subtitle = "Verteilung über semantisches Differential") + 
  labs(x = NULL, #"Subskalen",
       y = "Skala") +
  guides(color = F, linetype = F, shape = F, size = F, alpha = F, fill = F) +
  
  ## Theme
  theme_thesis()

plot(plot_subscales_post)

ggsave(filename = figureFileName("ds_subskalen"),
       plot = plot_subscales_post,
       path = figurePath(),
       width = 16,
       height = 7,
       units = "cm",
       dpi = 1000,
       type = "cairo-png")


